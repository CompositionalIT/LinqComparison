module HardCodedTests

open Expecto
open System.Linq

module Functions =
    /// A helper function that will track calls to any higher order function passed into another
    /// function.
    let trackCalls higherOrderFunc func data =
        let key = obj()
        let mutable count = 0
        let higherOrderFunc input =
            lock key (fun () -> count <- count + 1)
            higherOrderFunc input
        {| Result = func(data, higherOrderFunc); CallCount = count |}

/// Safely calls any function given some argument, converting the exception into Error<exn>
let safely thunk arg =
    try Ok(thunk arg)
    with ex -> Error ex

/// Compares two arbitrary functions that take in two arguments: an array of data, and some
/// higher-order function e.g. a predicate or mapper etc.. It records the number of times that
/// the higher-order function was called and returns the answer.
let compare netCoreFunc netFrameworkFunc after (inputData:int array) hof =
    let actual = inputData |> safely (Functions.trackCalls hof netCoreFunc >> (fun x -> {| x with Result = after x.Result |} ))
    let expected = inputData |> safely (Functions.trackCalls hof netFrameworkFunc >> (fun x -> {| x with Result = after x.Result |}))
    match actual, expected with
    | Error _, Error _ ->
        true
    | _ ->
        Expect.equal actual expected "Both results should be the same"
        true

[<Tests>]
let allTests =
    let compareQuick a b = compare a b id
    let deterministic a = compare a a id

    testList "Hard Coded" [
        testList "Deterministic" [
            testList ".NET Core" [
                testProperty "FirstOrDefault is deterministic" <| deterministic Enumerable.FirstOrDefault
                testProperty "LastOrDefault is deterministic" <| deterministic Enumerable.LastOrDefault
                testProperty "SingleOrDefault is deterministic" <| deterministic Enumerable.SingleOrDefault
                testProperty "Select is deterministic" <| compare Enumerable.Select Enumerable.Select Seq.toArray
                testProperty "Where is deterministic" <| compare Enumerable.Where Enumerable.Where Seq.toArray
            ]
            testList ".NET Framework" [
                testProperty "FirstOrDefault is deterministic" <| deterministic OldEnumerable.FirstOrDefault
                testProperty "LastOrDefault is deterministic" <| deterministic OldEnumerable.LastOrDefault
                testProperty "SingleOrDefault is deterministic" <| deterministic OldEnumerable.SingleOrDefault
                testProperty "Select is deterministic" <| compare OldEnumerable.Select OldEnumerable.Select Seq.toArray
                testProperty "Where is deterministic" <| compare OldEnumerable.Where OldEnumerable.Where Seq.toArray
            ]
        ]
        testList "Comparing Core and Framework" [
            let (=>) orderer b (data, hof) =
                b(orderer(data, (fun a -> a)), hof)
            testList "Single method" [
                testProperty "FirstOrDefault has not changed" <| compareQuick Enumerable.FirstOrDefault OldEnumerable.FirstOrDefault
                testProperty "LastOrDefault has not changed" <| compareQuick Enumerable.LastOrDefault OldEnumerable.LastOrDefault
                testProperty "SingleOrDefault has not changed" <| compareQuick Enumerable.SingleOrDefault OldEnumerable.SingleOrDefault
                testProperty "Select has not changed" <| compare Enumerable.Select OldEnumerable.Select Seq.toArray
                testProperty "Where has not changed" <| compare Enumerable.Where OldEnumerable.Where Seq.toArray
                testProperty "OrderBy has not changed" <| compare Enumerable.OrderBy OldEnumerable.OrderBy Seq.toArray
            ]
            testList "Run on OrderedEnumerable" [
                testProperty "FirstOrDefault has not changed" <| compare (Enumerable.OrderBy => Enumerable.FirstOrDefault) (OldEnumerable.OrderBy => OldEnumerable.FirstOrDefault) id
                testProperty "LastOrDefault has not changed" <| compare (Enumerable.OrderBy => Enumerable.LastOrDefault) (OldEnumerable.OrderBy => OldEnumerable.LastOrDefault) id
                testProperty "SingleOrDefault has not changed" <| compare (Enumerable.OrderBy => Enumerable.SingleOrDefault) (OldEnumerable.OrderBy => OldEnumerable.SingleOrDefault) id
                testProperty "Select has not changed" <| compare (Enumerable.OrderBy => Enumerable.Select) (OldEnumerable.OrderBy => OldEnumerable.Select) Seq.toArray
                testProperty "Where has not changed" <| compare (Enumerable.OrderBy => Enumerable.Where) (OldEnumerable.OrderBy => OldEnumerable.Where) Seq.toArray
            ]
        ]
    ]
