open Expecto
open System.Linq
open System.Collections.Generic
open FsCheck

module Functions =
    /// A helper function that will track calls to any higher order function passed into another
    /// function.
    let trackCalls higherOrderFunc func data =
        let key = obj()
        let mutable count = 0
        let predicate input =
            lock key (fun () -> count <- count + 1)
            higherOrderFunc input
        func(data, predicate), count

/// Safely calls any function given some argument, converting the exception into Error<exn>
let safely thunk arg =
    try Ok(thunk arg)
    with ex -> Error ex

let testProp a b = testPropertyWithConfigStdGen (0, 0) FsCheckConfig.defaultConfig a b

[<Tests>]
let myTests =
    /// Compares two arbitrary functions that take in two arguments: an array of data, and some
    /// higher-order function e.g. a predicate or mapper etc.. It records the number of times that
    /// the higher-order function was called and returns the answer.
    let quickCompare netCoreFunc netFrameworkFunc before after (inputData:_ array) higherOrderFunction =
        let newResult = inputData |> before |> safely (Functions.trackCalls higherOrderFunction netCoreFunc >> (fun (a,b) -> after a, b))
        let oldResult = inputData |> before |> safely (Functions.trackCalls higherOrderFunction netFrameworkFunc >> (fun (a,b) -> after a, b))
        match newResult, oldResult with
        | Error _, Error _ ->
            true
        | _ ->
            Expect.equal newResult oldResult "Both results should be the same"
            true

    testList "All" [
        testList "Deterministic tests" [
            testList ".NET Core" [
                testProp "FirstOrDefault is deterministic" <| quickCompare Enumerable.FirstOrDefault Enumerable.FirstOrDefault id id
                testProp "LastOrDefault is deterministic" <| quickCompare Enumerable.LastOrDefault Enumerable.LastOrDefault id id
                testProp "SingleOrDefault is deterministic" <| quickCompare Enumerable.SingleOrDefault Enumerable.SingleOrDefault id id
                testProp "Select is deterministic" <| quickCompare Enumerable.Select Enumerable.Select id Seq.toArray
            ]
            testList ".NET Framework" [
                testProp "FirstOrDefault is deterministic" <| quickCompare OldEnumerable.FirstOrDefault OldEnumerable.FirstOrDefault id id
                testProp "LastOrDefault is deterministic" <| quickCompare OldEnumerable.LastOrDefault OldEnumerable.LastOrDefault id id
                testProp "SingleOrDefault is deterministic" <| quickCompare OldEnumerable.SingleOrDefault OldEnumerable.SingleOrDefault id id
                testProp "Select is deterministic" <| quickCompare OldEnumerable.Select OldEnumerable.Select id Seq.toArray
            ]
        ]
        testList "Comparing .NET Core to .NET Framework" [
            testList "Single method" [
                testProp "FirstOrDefault has not changed" <| quickCompare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault id id
                testProp "LastOrDefault has not changed" <| quickCompare Enumerable.LastOrDefault OldEnumerable.LastOrDefault id id
                testProp "SingleOrDefault has not changed" <| quickCompare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault id id
                testProp "Select has not changed" <| quickCompare Enumerable.Select OldEnumerable.Select id Seq.toArray
            ]
            testList "OrderedEnumerable" [
                let orderIt (x:int array) = x.OrderBy(fun x -> x)
                testProp "FirstOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault orderIt id
                testProp "LastOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.LastOrDefault OldEnumerable.LastOrDefault orderIt id
                testProp "SingleOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault orderIt id
                testProp "Select has not changed on OrderedEnumerable" <| quickCompare Enumerable.Select OldEnumerable.Select orderIt Seq.toArray
            ]
        ]
    ]

[<EntryPoint>]
let main _ =
    printfn "Running tests!"
    runTests { defaultConfig with verbosity = Logging.LogLevel.Debug } myTests
