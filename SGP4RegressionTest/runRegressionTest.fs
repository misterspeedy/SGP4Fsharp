module SGP4RegressionTest

open System
open System.IO
open testfsharp

[<EntryPoint>]
let main argv =
    let testDir = @"C:\Code\VS2012\SGP4\SGP4Fsharp\Data\Testdata"
    // Execute the main 'test' program, which produces a .out file:
    runTests testDir
    // Compare the .out file with a reference file produced before the F# program was refactored:
    let returnCode = 
        if compareFiles.compare (Path.Combine(testDir, "tfsharpver.out")) (Path.Combine(testDir, "tfsharpver_reference.out")) then
            Console.ForegroundColor <- System.ConsoleColor.Green
            Console.WriteLine("Regression test passed")
            0
        else
            Console.ForegroundColor <- System.ConsoleColor.Red
            Console.WriteLine("Regression test failed")
            1

    Console.ForegroundColor <- ConsoleColor.White
    Console.WriteLine("Press a key")
    Console.ReadKey(true) |> ignore
    returnCode