#r @"C:\Users\Isaac\.nuget\packages\FsCheck\2.14.2\lib\netstandard2.0\fscheck.dll"
#r @"oldlinq/bin/Debug/netstandard2.1/OldLinq.dll"

open FsCheck
open System
open System.Collections.Generic
open System.Linq

let trackCalls predicate func data =
    let mutable count = 0
    let predicate input =
        count <- count + 1
        predicate input
    try func(data, predicate) |> ignore with _ -> ()
    count

trackCalls (fun _ -> true) Enumerable.LastOrDefault [0;1]
trackCalls (fun _ -> true) OldEnumerable.LastOrDefault [0;1]