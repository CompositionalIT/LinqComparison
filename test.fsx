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

type Transformer = IEnumerable<int> -> IEnumerable<int> * int
type Safe<'T> = Result<'T, exn>
type Aggregator = IEnumerable<int> -> Safe<int * int>
type Transform = Transform of name:string * old:Transformer * current:Transformer
type Aggregate = Aggregate of name:string * old:Aggregator * current:Aggregator

module Functions =
    let trackCalls predicate func data =
        let mutable count = 0
        let higherOrderFunction input =
            count <- count + 1
            predicate input
        func(data, higherOrderFunction), count

    let safely func arg : _ Safe = try Ok(func arg) with ex -> Error ex
    let makeAggregator name current old (Fun hof) =
        Aggregate(
            name,
            (fun data -> data |> safely (trackCalls hof current)),
            (fun data -> data |> safely (trackCalls hof old)))

    let makeCollector name current old (Fun hof) =
        Transform(
            name,
            (fun data -> trackCalls hof current data),
            (fun data -> trackCalls hof old data)
        )

    let firstOrDefault = makeAggregator "FirstOrDefault" Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault
    let lastOrDefault = makeAggregator "LastOrDefault" Enumerable.LastOrDefault OldEnumerable.LastOrDefault
    let singleOrDefault = makeAggregator "SingleOrDefault" Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault
    let count = makeAggregator "Count" Enumerable.Count OldEnumerable.Count

    let makeSelect = makeCollector "Select" Enumerable.Select OldEnumerable.Select
    let makeWhere = makeCollector "Where" Enumerable.Where OldEnumerable.Where
    let makeOrderBy = makeCollector "OrderBy" (fun (a, b:int -> bool) -> Enumerable.OrderBy(a, fun a -> a) :> _) (fun (a, _) -> OldEnumerable.OrderBy(a, fun a -> a) :> _)

let generateAggregator =
    let aggregator =
        [ Functions.firstOrDefault
          Functions.lastOrDefault
          Functions.singleOrDefault
          Functions.count ]
        |> Gen.elements

    gen {
        let! hof = genHof()
        let! aggregator = aggregator
        return aggregator hof
    }

let generateCollector =
    [ genHof() |> Gen.map Functions.makeSelect
      genHof() |> Gen.map Functions.makeOrderBy
      genHof() |> Gen.map Functions.makeWhere ]
    |> Gen.oneof

type FunctionsGen =
    static member Aggregator() =
        { new Arbitrary<Aggregate>() with override _.Generator = generateAggregator }
    static member Collector() =
        { new Arbitrary<Transform>() with override _.Generator = generateCollector }

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

let collectOnly collectors (data:_ array) =
    let newPipeline, oldPipeline =
        collectors
        |> List.map(fun (Transform(_,b,c)) -> b,c)
        |> List.unzip
    let newResult = data |> executePipeline newPipeline |> (fun (a,b) -> Seq.toArray a, b)
    let oldResult = data |> executePipeline oldPipeline |> (fun (a,b) -> Seq.toArray a, b)

    newResult = oldResult

let collectAndAggregate collectors (Aggregate (_, newAgg, oldAgg)) (NonEmptyArray data) =
    let newPipeline, oldPipeline =
        collectors
        |> List.map(fun (Transform(_,b,c)) -> b,c)
        |> List.unzip
    let newResult = data |> executePipeline newPipeline |> executeAggregator newAgg |> Result.mapError ignore
    let oldResult = data |> executePipeline oldPipeline |> executeAggregator oldAgg |> Result.mapError ignore

    newResult = oldResult

Check.One( { Config.Default with Replay = Some (Random.newSeed()); Arbitrary = [ typeof<FunctionsGen> ] }, collectAndAggregate)