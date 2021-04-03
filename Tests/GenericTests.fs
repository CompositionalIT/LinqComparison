module GenericTests

open FsCheck
open System
open System.Linq
open Expecto.ExpectoFsCheck
open Expecto

type Identity = Func<int, int>
type Predicate = Func<int, bool>
type CallResult<'T> = { Result : 'T; Calls : int }

[<AutoOpen>]
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
        member this.Create() =
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
        member this.Create() =
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

let generatePipelines (mappers:Functions.Mapper list) =
    mappers
    |> List.map(fun mapper -> mapper.Create())
    |> List.unzip

let collectOnly mappers (data:int array) =
    let newPipeline, oldPipeline = generatePipelines mappers
    let newResult = data |> executePipeline newPipeline |> (fun output -> { output with Result = output.Result |> Seq.toArray })
    let oldResult = data |> executePipeline oldPipeline |> (fun output -> { output with Result = output.Result |> Seq.toArray })
    Expect.equal newResult oldResult "Results should be the same"

type GeneratedInputs =
    { Data : int NonEmptyArray // generated input data
      Mappers : Mapper list    // generated list of mappers
      Aggregator: Aggregator } // generated aggregator

let collectAndAggregate (input:GeneratedInputs) =
    let newPipeline, oldPipeline = generatePipelines input.Mappers
    let newAggregator, oldAggregator = input.Aggregator.Create()

    let newResult = input.Data.Get |> executePipeline newPipeline |> executeAggregator newAggregator |> Result.mapError ignore
    let oldResult = input.Data.Get |> executePipeline oldPipeline |> executeAggregator oldAggregator |> Result.mapError ignore

    Expect.equal newResult oldResult "Results should be the same"

let compositionalComparison (input:int NonEmptyArray, predicate:PredicateFunction) =
    let orderByFunc = Function<_,_>.From id
    let longMappers, _ = [ OrderBy orderByFunc; Where predicate; ] |> generatePipelines
    let longResult = input.Get |> executePipeline longMappers |> fun a -> Ok { Calls = a.Calls; Result = a.Result.FirstOrDefault() }

    let shortMappers, _ = [ OrderBy orderByFunc ] |> generatePipelines
    let shortAggregator, _ =
        let firstOrDefault = FirstOrDefault predicate
        firstOrDefault.Create()
    let shortResult = input.Get |> executePipeline shortMappers |> executeAggregator shortAggregator

    Expect.equal longResult shortResult "Should have same Result"

let allTests = testList "Generic" [
    testProperty "Collection Methods" collectOnly
    testProperty "Collect and Aggregation Methods" collectAndAggregate
    testProperty "Composing Differently, Same Result" compositionalComparison
]