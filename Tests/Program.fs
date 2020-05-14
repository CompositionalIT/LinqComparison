open Expecto
open System
open System.Linq
open System.Collections.Generic

open FsCheck
open System
open System.Collections.Generic
open System.Linq

[<AutoOpen>]
module Generation =
    type Identity = Func<int, int>
    type Predicate = Func<int, bool>
    let genIdentity = Arb.Default.Fun<int, int>() |> Arb.toGen
    let genPredicate = Arb.Default.Fun<int, bool>() |> Arb.toGen

type CollectionMapper = IEnumerable<int> -> IEnumerable<int>
type Aggregation = IEnumerable<int> -> int

type ComparisonMethod =
    | CollectionComparison of old:CollectionMapper * current:CollectionMapper
    | AggregationComparison of name:string * old:Aggregation * current:Aggregation

module Functions =
    let trackCalls predicate func data =
        let key = obj()
        let mutable count = 0
        let predicate input =
            lock key (fun () -> count <- count + 1)
            predicate input
        func(data, predicate) |> ignore
        count

    let makeAggregation name current old predicate =
        AggregationComparison(
            name,
            (fun data -> trackCalls predicate current data),
            (fun data -> trackCalls predicate old data))

    let firstOrDefault = makeAggregation "FirstOrDefault" Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault
    let lastOrDefault = makeAggregation "LastOrDefault" Enumerable.LastOrDefault OldEnumerable.LastOrDefault
    let singleOrDefault = makeAggregation "SingleOrDefault" Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault

let generateComparison =
    let aggregator =
        [ Functions.firstOrDefault
          Functions.lastOrDefault
          Functions.singleOrDefault ]
        |> Gen.elements

    gen {
        let! predicate = genPredicate
        let! aggregator = aggregator
        return aggregator predicate
    }

type FunctionsGen =
    static member Aggregator() =
        { new Arbitrary<ComparisonMethod>() with
            override _.Generator = generateComparison
        }

let safely thunk x =
    try Ok(thunk x)
    with ex -> Error ex

type CollectionMapper<'TSource> = IEnumerable<'TSource> -> IEnumerable<'TSource>
type Aggregation<'TSource> = IEnumerable<'TSource> -> 'TSource

type ComparisonMethod<'TSource> =
    | CollectionComparison of old:CollectionMapper<'TSource> * current:CollectionMapper<'TSource>
    | AggregationComparison of old:Aggregation<'TSource> * current:Aggregation<'TSource>

[<Tests>]
let myTests =
    let quickCompare netCoreFunc netFrameworkFunc prepare (inputData:int array) predicate =
        let newResult = inputData |> prepare |> safely (Functions.trackCalls predicate netCoreFunc)
        let oldResult = inputData |> prepare |> safely (Functions.trackCalls predicate netFrameworkFunc)
        match newResult, oldResult with
        | Ok a, Ok b -> a = b
        | Error _, Error _ -> true
        | _ -> false

    testList "LINQ tests" [
        testProperty "New FirstOrDefault is deterministic" <| quickCompare Enumerable.FirstOrDefault Enumerable.FirstOrDefault id
        testProperty "New LastOrDefault is deterministic" <| quickCompare Enumerable.LastOrDefault Enumerable.LastOrDefault id
        testProperty "New SingleOrDefault is deterministic" <| quickCompare Enumerable.SingleOrDefault Enumerable.SingleOrDefault id
        testProperty "Original FirstOrDefault is deterministic" <| quickCompare OldEnumerable.FirstOrDefault OldEnumerable.FirstOrDefault id
        testProperty "Original LastOrDefault is deterministic" <| quickCompare OldEnumerable.LastOrDefault OldEnumerable.LastOrDefault id
        testProperty "Original SingleOrDefault is deterministic" <| quickCompare OldEnumerable.SingleOrDefault OldEnumerable.SingleOrDefault id

        testProperty "FirstOrDefault has not changed, in isolation" <| quickCompare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault id
        testProperty "LastOrDefault has not changed, in isolation" <| quickCompare Enumerable.LastOrDefault OldEnumerable.LastOrDefault id
        testProperty "SingleOrDefault has not changed, in isolation" <| quickCompare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault id

        let orderIt (x:int array) = x.OrderBy(fun x -> x)
        testProperty "FirstOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault orderIt
        testProperty "LastOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.LastOrDefault OldEnumerable.LastOrDefault orderIt
        testProperty "SingleOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault orderIt
    ] |> testSequenced

[<EntryPoint>]
let main _ =
    printfn "Running tests!"
    runTests { defaultConfig with verbosity = Logging.LogLevel.Debug } myTests
