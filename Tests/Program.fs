open Expecto

let allTests = testList "All Tests" [
    HardCodedTests.allTests
    GenericTests.allTests
]

[<EntryPoint>]
let main _ =
    printfn "Running tests!"
    let defaultConfig = { defaultConfig with fsCheckMaxTests = 500; verbosity = Logging.LogLevel.Debug }
    runTests defaultConfig allTests