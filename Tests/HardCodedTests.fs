module HardCodedTests

open Expecto
open System.Linq
open FsCheck

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

type LinqCompare =
    /// Compares two arbitrary functions that take in two arguments: an array of data, and some
    /// higher-order function e.g. a predicate or mapper etc.. It records the number of times that
    /// the higher-order function was called and returns the answer.
    static member check (netCoreFunc, netFrameworkFunc, after) =
        fun (inputData:int array) (hof:Function<int, _>) ->
            let actual = inputData |> safely (Functions.trackCalls hof.Value netCoreFunc >> (fun x -> {| x with Result = after x.Result |} ))
            let expected = inputData |> safely (Functions.trackCalls hof.Value netFrameworkFunc >> (fun x -> {| x with Result = after x.Result |}))
            match actual, expected with
            | Error _, Error _ ->
                true
            | _ ->
                Expect.equal actual expected "Both results should be the same"
                true
    static member check (a, ?b) = LinqCompare.check(a, b |> Option.defaultValue a, id)

[<Tests>]
let allTests =
    testList "Hard Coded" [
        testList "Deterministic" [
            testList ".NET Core" [
                testProperty "FirstOrDefault is deterministic" <| LinqCompare.check Enumerable.FirstOrDefault
                testProperty "LastOrDefault is deterministic" <| LinqCompare.check Enumerable.LastOrDefault
                testProperty "SingleOrDefault is deterministic" <| LinqCompare.check Enumerable.SingleOrDefault
                testProperty "Select is deterministic" <| LinqCompare.check (Enumerable.Select, Enumerable.Select, Seq.toArray)
                testProperty "Where is deterministic" <| LinqCompare.check (Enumerable.Where, Enumerable.Where, Seq.toArray)
            ]
            testList ".NET Framework" [
                testProperty "FirstOrDefault is deterministic" <| LinqCompare.check OldEnumerable.FirstOrDefault
                testProperty "LastOrDefault is deterministic" <| LinqCompare.check OldEnumerable.LastOrDefault
                testProperty "SingleOrDefault is deterministic" <| LinqCompare.check OldEnumerable.SingleOrDefault
                testProperty "Select is deterministic" <| LinqCompare.check (OldEnumerable.Select, OldEnumerable.Select, Seq.toArray)
                testProperty "Where is deterministic" <| LinqCompare.check (OldEnumerable.Where, OldEnumerable.Where, Seq.toArray)
            ]
        ]
        testList "Comparing Core and Framework" [
            let (=>) orderer b (data, hof) = b(orderer(data, id), hof)
            testList "Single method" [
                testProperty "FirstOrDefault has not changed" <| LinqCompare.check (Enumerable.FirstOrDefault, OldEnumerable.FirstOrDefault)
                testProperty "LastOrDefault has not changed" <| LinqCompare.check (Enumerable.LastOrDefault, OldEnumerable.LastOrDefault)
                testProperty "SingleOrDefault has not changed" <| LinqCompare.check (Enumerable.SingleOrDefault, OldEnumerable.SingleOrDefault)
                testProperty "Select has not changed" <| LinqCompare.check (Enumerable.Select, OldEnumerable.Select, Seq.toArray)
                testProperty "Where has not changed" <| LinqCompare.check (Enumerable.Where, OldEnumerable.Where, Seq.toArray)
                testProperty "OrderBy has not changed" <| LinqCompare.check (Enumerable.OrderBy, OldEnumerable.OrderBy, Seq.toArray)
            ]
            testList "Run on OrderedEnumerable" [
                testProperty "FirstOrDefault has not changed" <| LinqCompare.check (Enumerable.OrderBy => Enumerable.FirstOrDefault, OldEnumerable.OrderBy => OldEnumerable.FirstOrDefault)
                testProperty "LastOrDefault has not changed" <| LinqCompare.check (Enumerable.OrderBy => Enumerable.LastOrDefault, OldEnumerable.OrderBy => OldEnumerable.LastOrDefault)
                testProperty "SingleOrDefault has not changed" <| LinqCompare.check (Enumerable.OrderBy => Enumerable.SingleOrDefault, OldEnumerable.OrderBy => OldEnumerable.SingleOrDefault)
                testProperty "Select has not changed" <| LinqCompare.check (Enumerable.OrderBy => Enumerable.Select, OldEnumerable.OrderBy => OldEnumerable.Select, Seq.toArray)
                testProperty "Where has not changed" <| LinqCompare.check (Enumerable.OrderBy => Enumerable.Where, OldEnumerable.OrderBy => OldEnumerable.Where, Seq.toArray)
            ]
        ]
    ]
