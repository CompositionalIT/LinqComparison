module GenericTests

open FsCheck
open System
open System.Linq
open Expecto.ExpectoFsCheck
open Expecto

type Identity = Func<int, int>
type Predicate = Func<int, bool>
type CallResult<'T> = { Result : 'T; Calls : int }

module Functions =
    let trackCalls predicate func data =
        let mutable count = 0
        let higherOrderFunction input =
            count <- count + 1
            predicate input
        { Result = func(data, higherOrderFunction)
          Calls = count }

    let safely func arg : Result<'T, exn> = try Ok(func arg) with ex -> Error ex
    type PredicateFunction = Function<int, bool>
    type IdentityFunction = Function<int, int>
    let makeAggregator hof current old =
        (fun data -> data |> safely (trackCalls hof current)),
        (fun data -> data |> safely (trackCalls hof old))
    let makeMapper hof current old =
        (fun data -> trackCalls hof current data),
        (fun data -> trackCalls hof old data)
    type Aggregator =
        | FirstOrDefault of PredicateFunction
        | LastOrDefault of PredicateFunction
        | SingleOrDefault of PredicateFunction
        | Count of PredicateFunction
        member this.Gen() =
            match this with
            | FirstOrDefault (Fun f) -> makeAggregator f Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault
            | LastOrDefault (Fun f) -> makeAggregator f Enumerable.LastOrDefault OldEnumerable.LastOrDefault
            | SingleOrDefault (Fun f) -> makeAggregator f Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault
            | Count (Fun f) -> makeAggregator f Enumerable.Count OldEnumerable.Count

    type Mapper =
        | Select of IdentityFunction
        | Where of PredicateFunction
        | OrderBy of IdentityFunction
        | SelectMany of Function<int,int array>
        | Take of int
        member this.Gen() =
            match this with
            | Select (Fun f) -> makeMapper f Enumerable.Select OldEnumerable.Select
            | Where (Fun f) -> makeMapper f Enumerable.Where OldEnumerable.Where
            | OrderBy (Fun f) -> makeMapper f (fun (data,b) -> Enumerable.OrderBy(data, (fun value -> b value)) :> _) (fun (data,b) -> OldEnumerable.OrderBy(data, (fun value -> b value)) :> _)
            | SelectMany (Fun f) -> makeMapper (f >> Array.toSeq) Enumerable.SelectMany OldEnumerable.SelectMany
            | Take n -> (fun data -> { Result = Enumerable.Take(data, n); Calls = 0 }), (fun data -> { Result = OldEnumerable.Take(data, n); Calls = 0 })

let executePipeline collectors (data:_ seq) =
    collectors
    |> Seq.fold(fun state collector ->
        let output = collector state.Result
        { output with Calls = output.Calls + state.Calls }) { Result = data; Calls = 0 }

let executeAggregator aggregator output =
    match aggregator output.Result with
    | Ok result -> Ok { result with Calls = result.Calls + output.Calls }
    | Error x -> Error x

let mergeMappers (mappers:Functions.Mapper list) =
    mappers
    |> List.map(fun mapper -> mapper.Gen())
    |> List.unzip

let collectOnly mappers (data:_ array) =
    let newPipeline, oldPipeline = mergeMappers mappers
    let newResult = data |> executePipeline newPipeline |> (fun output -> { output with Result = output.Result |> Seq.toArray })
    let oldResult = data |> executePipeline oldPipeline |> (fun output -> { output with Result = output.Result |> Seq.toArray })
    Expect.equal newResult oldResult "Results should be the same"

let collectAndAggregate (input:{| Data : _ NonEmptyArray; Mappers : Functions.Mapper list; Aggregator:Functions.Aggregator |}) =
    let newPipeline, oldPipeline = mergeMappers input.Mappers
    let newAgg, oldAgg = input.Aggregator.Gen()

    let newResult = input.Data.Get |> executePipeline newPipeline |> executeAggregator newAgg |> Result.mapError ignore
    let oldResult = input.Data.Get |> executePipeline oldPipeline |> executeAggregator oldAgg |> Result.mapError ignore

    Expect.equal newResult oldResult "Results should be the same"

let allTests = testList "Generic" [
    testProperty "Collection Methods" collectOnly
    testProperty "Collect and Aggregation Methods" collectAndAggregate
]