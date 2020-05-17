#r @"C:\Users\Isaac\.nuget\packages\FsCheck\2.14.2\lib\netstandard2.0\fscheck.dll"
#r @"oldlinq/bin/Debug/netstandard2.1/OldLinq.dll"

open FsCheck
open System
open System.Collections.Generic
open System.Linq

[<AutoOpen>]
module Generation =
    type Identity = Func<int, int>
    type Predicate = Func<int, bool>
    let genIdentity<'T when 'T : equality> = Arb.Default.Fun<'T, 'T>() |> Arb.toGen
    let inline genHof() = Arb.Default.Function() |> Arb.toGen
    let getFun (Fun f) = f

type Safe<'T> = Result<'T, exn>

module Functions =
    let trackCalls predicate func data =
        let mutable count = 0
        let higherOrderFunction input =
            count <- count + 1
            predicate input
        func(data, higherOrderFunction), count

    let safely func arg : _ Safe = try Ok(func arg) with ex -> Error ex
    type PredicateFun = Function<int, bool>
    type IdentityFun = Function<int, int>
    let makeAggregator hof current old =
        (fun data -> data |> safely (trackCalls hof current)),
        (fun data -> data |> safely (trackCalls hof old))
    let makeMapper hof current old =
        (fun data -> trackCalls hof current data),
        (fun data -> trackCalls hof old data)
    type Aggregator =
        | FirstOrDefault of PredicateFun
        | LastOrDefault of PredicateFun
        | SingleOrDefault of PredicateFun
        | Count of PredicateFun
        member this.Gen() =
            match this with
            | FirstOrDefault (Fun f) -> makeAggregator f Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault
            | LastOrDefault (Fun f) -> makeAggregator f Enumerable.LastOrDefault OldEnumerable.LastOrDefault
            | SingleOrDefault (Fun f) -> makeAggregator f Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault
            | Count (Fun f) -> makeAggregator f Enumerable.Count OldEnumerable.Count

    type Mapper =
        | Select of IdentityFun
        | Where of PredicateFun
        | OrderBy of IdentityFun
        member this.Gen() =
            match this with
            | Select (Fun f) -> makeMapper f Enumerable.Select OldEnumerable.Select
            | Where (Fun f) -> makeMapper f Enumerable.Where OldEnumerable.Where
            | OrderBy (Fun f) -> makeMapper f (fun (data,b) -> Enumerable.OrderBy(data, (fun value -> b value)) :> _) (fun (data,b) -> OldEnumerable.OrderBy(data, (fun value -> b value)) :> _)

let executePipeline collectors (data:_ seq) =
    collectors
    |> Seq.fold(fun (data, count) collector ->
        let result, additional = collector data
        result, count + additional)
        (data, 0)

let executeAggregator aggregator (collectorResult, collectorCount) =
    match collectorResult |> aggregator with
    | Ok (aggregatorResult, aggregationCount) -> Ok(aggregatorResult, (aggregationCount + collectorCount))
    | Error x -> Error x

let collectOnly (mappers:Functions.Mapper list) (data:_ array) =
    let newPipeline, oldPipeline =
        mappers
        |> List.map(fun mapper -> mapper.Gen())
        |> List.unzip
    let newResult = data |> executePipeline newPipeline |> (fun (a,b) -> Seq.toArray a, b)
    let oldResult = data |> executePipeline oldPipeline |> (fun (a,b) -> Seq.toArray a, b)

    newResult = oldResult

let collectAndAggregate (input:{| Data : _ NonEmptyArray; Mappers : Functions.Mapper list; Aggregator:Functions.Aggregator |}) =
    let newPipeline, oldPipeline =
        input.Mappers
        |> List.map (fun mapper -> mapper.Gen())
        |> List.unzip
    let newAgg, oldAgg = input.Aggregator.Gen()

    let newResult = input.Data.Get |> executePipeline newPipeline |> executeAggregator newAgg |> Result.mapError ignore
    let oldResult = input.Data.Get |> executePipeline oldPipeline |> executeAggregator oldAgg |> Result.mapError ignore

    newResult = oldResult

Check.One({ Config.Default with Replay = Some (Random.newSeed()) }, collectAndAggregate)