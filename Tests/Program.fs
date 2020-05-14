open Expecto
open System.Linq
open System.Collections.Generic
open FsCheck

module Functions =
    let trackCalls predicate func data =
        let key = obj()
        let mutable count = 0
        let predicate input =
            lock key (fun () -> count <- count + 1)
            predicate input
        func(data, predicate) |> ignore
        count

let safely thunk x =
    try Ok(thunk x)
    with ex -> Error ex

[<Tests>]
let myTests =
    /// Compares two arbitrary functions that take in two arguments: an array of data, and some
    /// higher-order function e.g. a predicate or mapper etc.. It records the number of times that
    /// the higher-order function was called and returns the answer.
    let quickCompare netCoreFunc netFrameworkFunc prepare (inputData:_ array) higherOrderFunction =
        let newResult = inputData |> prepare |> safely (Functions.trackCalls higherOrderFunction netCoreFunc)
        let oldResult = inputData |> prepare |> safely (Functions.trackCalls higherOrderFunction netFrameworkFunc)
        match newResult, oldResult with
        | Error _, Error _ ->
            true
        | _ ->
            Expect.equal newResult oldResult "Both results should be the same!"
            true

    testList "All" [
        testList "Deterministic tests" [
            testProperty "New FirstOrDefault is deterministic" <| quickCompare Enumerable.FirstOrDefault Enumerable.FirstOrDefault id
            testProperty "New LastOrDefault is deterministic" <| quickCompare Enumerable.LastOrDefault Enumerable.LastOrDefault id
            testProperty "New SingleOrDefault is deterministic" <| quickCompare Enumerable.SingleOrDefault Enumerable.SingleOrDefault id
            testProperty "New Select is deterministic" <| quickCompare Enumerable.Select Enumerable.Select id
            testProperty "Original FirstOrDefault is deterministic" <| quickCompare OldEnumerable.FirstOrDefault OldEnumerable.FirstOrDefault id
            testProperty "Original LastOrDefault is deterministic" <| quickCompare OldEnumerable.LastOrDefault OldEnumerable.LastOrDefault id
            testProperty "Original SingleOrDefault is deterministic" <| quickCompare OldEnumerable.SingleOrDefault OldEnumerable.SingleOrDefault id
            testProperty "Old Select is deterministic" <| quickCompare OldEnumerable.Select OldEnumerable.Select id
        ] |> testSequenced
        testList "Comparing .NET Core to .NET Framework" [
            testProperty "FirstOrDefault has not changed" <| quickCompare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault id
            testProperty "LastOrDefault has not changed" <| quickCompare Enumerable.LastOrDefault OldEnumerable.LastOrDefault id
            testProperty "SingleOrDefault has not changed" <| quickCompare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault id
            testProperty "Select has not changed" <| quickCompare Enumerable.Select OldEnumerable.Select id

            let orderIt (x:int array) = x.OrderBy(fun x -> x)
            testProperty "FirstOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault orderIt
            testProperty "LastOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.LastOrDefault OldEnumerable.LastOrDefault orderIt
            testProperty "SingleOrDefault has not changed on OrderedEnumerable" <| quickCompare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault orderIt
            testProperty "Select has not changed on OrderedEnumerable" <| quickCompare Enumerable.Select OldEnumerable.Select orderIt
        ] |> testSequenced
    ] |> testSequenced

[<EntryPoint>]
let main _ =
    printfn "Running tests!"
    runTests { defaultConfig with verbosity = Logging.LogLevel.Debug } myTests
