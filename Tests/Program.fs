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
        {| Result = func(data, predicate); CallCount = count |}

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
    let compare netCoreFunc netFrameworkFunc before after (inputData:_ array) higherOrderFunction =
        let actual = inputData |> before |> safely (Functions.trackCalls higherOrderFunction netCoreFunc >> (fun x -> {| x with Result = after x.Result |} ))
        let expected = inputData |> before |> safely (Functions.trackCalls higherOrderFunction netFrameworkFunc >> (fun x -> {| x with Result = after x.Result |}))
        match actual, expected with
        | Error _, Error _ ->
            true
        | _ ->
            Expect.equal actual expected "Both results should be the same"
            true
    let compareQuick a b = compare a b id id
    let deterministic a = compare a a id id

    testList "All" [
        testList "Deterministic tests" [
            testList ".NET Core" [
                testProp "FirstOrDefault is deterministic" <| deterministic Enumerable.FirstOrDefault
                testProp "LastOrDefault is deterministic" <| deterministic Enumerable.LastOrDefault
                testProp "SingleOrDefault is deterministic" <| deterministic Enumerable.SingleOrDefault
                testProp "Select is deterministic" <| compare Enumerable.Select Enumerable.Select id Seq.toArray
                testProp "Where is deterministic" <| compare Enumerable.Where Enumerable.Where id Seq.toArray
            ]
            testList ".NET Framework" [
                testProp "FirstOrDefault is deterministic" <| deterministic OldEnumerable.FirstOrDefault
                testProp "LastOrDefault is deterministic" <| deterministic OldEnumerable.LastOrDefault
                testProp "SingleOrDefault is deterministic" <| deterministic OldEnumerable.SingleOrDefault
                testProp "Select is deterministic" <| compare OldEnumerable.Select OldEnumerable.Select id Seq.toArray
                testProp "Where is deterministic" <| compare OldEnumerable.Where OldEnumerable.Where id Seq.toArray
            ]
        ]
        testList "Comparing .NET Core to .NET Framework" [
            testList "Single method" [
                testProp "FirstOrDefault has not changed" <| compare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault id id
                testProp "LastOrDefault has not changed" <| compare Enumerable.LastOrDefault OldEnumerable.LastOrDefault id id
                testProp "SingleOrDefault has not changed" <| compare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault id id
                testProp "Select has not changed" <| compare Enumerable.Select OldEnumerable.Select id Seq.toArray
                testProp "Where has not changed" <| compare Enumerable.Where OldEnumerable.Where id Seq.toArray
            ]
            testList "OrderedEnumerable" [
                let toOrderable (x:int array) = x.OrderBy(fun x -> x)
                testProp "FirstOrDefault has not changed on OrderedEnumerable" <| compare Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault toOrderable id
                testProp "LastOrDefault has not changed on OrderedEnumerable" <| compare Enumerable.LastOrDefault OldEnumerable.LastOrDefault toOrderable id
                testProp "SingleOrDefault has not changed on OrderedEnumerable" <| compare Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault toOrderable id
                testProp "Select has not changed on OrderedEnumerable" <| compare Enumerable.Select OldEnumerable.Select toOrderable Seq.toArray
                testProp "Where has not changed on OrderedEnumerable" <| compare Enumerable.Where OldEnumerable.Where toOrderable Seq.toArray
            ]
        ]
    ]

[<EntryPoint>]
let main _ =
    printfn "Running tests!"
    runTests { defaultConfig with verbosity = Logging.LogLevel.Debug } myTests
