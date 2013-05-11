module sscanf

open System
// A minimal implementation of sscanf - just enough to read SGP4's element files

// "%2d %5ld %1c %10s %2d %12lf %11lf %7lf %2d %7lf %2d %2d %6ld "
//1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753
//2 00005  34.2682 348.7242 1859667 331.7664  19.3264 10.82419157413667     0.00      4320.0        360.00

//***here just rewrote sscanf - debugging
let sscanf (format : string) (input : string) =
    let digits str = 
        let digs = seq { for char in str do
                             if char >= '0' && char <= '9' then
                                 yield char }
                   |> Array.ofSeq
        new System.String(digs)

    let convert c str =
        match c with
        | "d"  -> Int32.Parse str |> box
        | "ld" -> Int64.Parse str |> box
        | "f"  -> Single.Parse str |> box
        | "lf" -> Double.Parse str |> box
        | "c"  -> str.[0] |> box
        | "s"  -> str |> box
        | _    -> failwithf "Unknown type string: %s" c

    let formats = format.Split([|'%'|], StringSplitOptions.RemoveEmptyEntries)
                  |> Array.map (fun item -> item.Trim())

    let lengths = formats 
                  |> Array.map (fun format -> digits format) 
                  |> Array.map (fun str -> if str <> "" then 
                                              Int32.Parse str
                                           else
                                              -1) // No specific length - read to next space

    let types = formats
                |> Array.map (fun format -> let pos = (digits format).Length
                                            format.Substring(pos))

    let mutable pos = 0
    let values = ResizeArray()

    for atype, length in (Array.zip types lengths) do
        let mutable str = ""
        let mutable finished = false
        let mutable anyNonSpaceRead = false
        let isNumeric = atype <> "c" && atype <> "s"
        while not finished do
            str <- str + string(input.[pos])
            if not (input.[pos] = ' ' || input.[pos] = '\t') then
                anyNonSpaceRead <- true
            pos <- pos+1
            // Stop if we read enough characters, or when no length specified we hit a space:
            finished <- (pos >= input.Length)
                        ||
                        ((length > -1) && (str.Length >= length))
                        ||  
                        ((length = -1) && isNumeric && anyNonSpaceRead && input.[pos] = ' ')
        let value = convert atype str
        values.Add(value)

    values

//    let types = formats
//                |> Array.map (fun format -> let pos = (digits format).Length
//                                            format.Substring(pos))
//
//    // TODO error handling on out of range
//    let raw = Array.zip lengths positions
//              |> Array.map (fun (len, pos) -> input.Substring(pos, len))
//
//    let values = Array.zip raw types 
//                |> Array.map (fun (rawItem, _type) -> convert _type rawItem)

//    values



//let sscanf (format : string) (input : string) =
//    let digits str = 
//        let digs = seq { for char in str do
//                             if char >= '0' && char <= '9' then
//                                 yield char }
//                   |> Array.ofSeq
//        new System.String(digs)
//
//    let convert c str =
//        match c with
//        | "d"  -> Int32.Parse str |> box
//        | "ld" -> Int64.Parse str |> box
//        | "f"  -> Single.Parse str |> box
//        | "lf" -> Double.Parse str |> box
//        | "c"  -> str.[0] |> box
//        | "s"  -> str |> box
//        | _    -> failwithf "Unknown type string: %s" c
//
//    let formats = format.Split([|'%'|], StringSplitOptions.RemoveEmptyEntries)
//                  |> Array.map (fun item -> item.Trim())
//
//    let lengths = formats 
//                  |> Array.map (fun format -> digits format) 
//                  |> Array.map Int32.Parse
//
//    let acc = ref 0
//    let positions = lengths
//                    |> Array.map (fun len -> let pos = !acc
//                                             acc := !acc + len
//                                             pos)
//
//    let types = formats
//                |> Array.map (fun format -> let pos = (digits format).Length
//                                            format.Substring(pos))
//
//    // TODO error handling on out of range
//    let raw = Array.zip lengths positions
//              |> Array.map (fun (len, pos) -> input.Substring(pos, len))
//
//    let values = Array.zip raw types 
//                |> Array.map (fun (rawItem, _type) -> convert _type rawItem)
//
//    values

let useit() =
    let sscanfResult = sscanf "1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753" "%2d %5ld %1c %10s %2d %12lf %11lf %7lf %2d %7lf %2d %2d %6ld "

    let v1 = sscanfResult.[0] :?> int 
    let v2 = sscanfResult.[1] :?> int64
    let v3 = sscanfResult.[2] :?> char
    let v4 = sscanfResult.[3] :?> string
    let v5 = sscanfResult.[4] :?> int
    let v6 = sscanfResult.[5] :?> double
    let v7 = sscanfResult.[6] :?> double
    let v8 = sscanfResult.[7] :?> double
    let v9 = sscanfResult.[8] :?> int
    let v10 = sscanfResult.[9] :?> double
    let v11 = sscanfResult.[10] :?> int
    let v12 = sscanfResult.[11] :?> int
    let v13 = sscanfResult.[12] :?> int64

    printfn "%i %il %c %s %i %f %f %f %i %f %i %i %il" v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13
