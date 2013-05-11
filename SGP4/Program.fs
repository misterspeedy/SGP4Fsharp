module Program

open System
open testfsharp

[<EntryPoint>]
let main argv = 
    runTests @"C:\Code\VS2012\SGP4\SGP4Fsharp\Data\Testdata"
    printf "Press a key"
    Console.ReadKey(true) |> ignore
    0 // return an integer exit code
