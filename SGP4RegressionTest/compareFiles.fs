module compareFiles

open System.IO

let compare file1 file2 =
    File.ReadAllText file1 = File.ReadAllText file2