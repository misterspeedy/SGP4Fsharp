namespace SGP4Tests

open System
open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``Given a format string and an input string``() =

    [<Test>]
    member x.``Performing a sscanf creates the right results``() =
        let caster1 (result : Collections.Generic.List<Object>) = 
                                               (
                                                    result.[0] :?> int,
                                                    result.[1] :?> int64,
                                                    result.[2] :?> char,
                                                    result.[3] :?> string,
                                                    result.[4] :?> int,
                                                    result.[5] :?> double,
                                                    result.[6] :?> double,
                                                    result.[7] :?> double,
                                                    result.[8] :?> int,
                                                    result.[9] :?> double,
                                                    result.[10] :?> int,
                                                    result.[11] :?> int,
                                                    result.[12] :?> int64
                                               ).ToString()
        let caster2 (result : Collections.Generic.List<Object>) = 
                                               (
                                                    result.[0] :?> int,
                                                    result.[1] :?> int64,
                                                    result.[2] :?> float,
                                                    result.[3] :?> float,
                                                    result.[4] :?> float,
                                                    result.[5] :?> float,
                                                    result.[6] :?> float,
                                                    result.[7] :?> float,
                                                    result.[8] :?> int64,
                                                    result.[9] :?> float,
                                                    result.[10] :?> float,
                                                    result.[11] :?> float                                                   
                                               ).ToString()
        let format1 = "%2d %5ld %1c %10s %2d %12lf %11lf %7lf %2d %7lf %2d %2d %6ld "
        // "%2d %5ld %9lf %9lf %8lf %9lf %9lf %11lf %6ld %lf %lf %lf", 
        let format2 = "%2d %5ld %9lf %9lf %8lf %9lf %9lf %12lf %6ld %lf %lf %lf" // 5th from last changed from 11 to 12

        let data = [| 
                      format1,
                      "1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753",
                      (1, 5L, 'U', " 58002B   ", 0, 179.78495062, 0.00000023, 0.0, 0, 28098.0, -4, 0, 4753).ToString(),
                      caster1

                      format1,
                      "1 04632U 70093B   04031.91070959 -.00000084  00000-0  10000-3 0  9955",
                      (1, 4632L, 'U', " 70093B   ", 4, 031.91070959, -0.00000084, 0.0, 0, 10000.0, -3, 0, 9955).ToString(),
                      caster1
                      
                      format2,
                      "2 00005  34.2682 348.7242.1859667 331.7664  19.3264 10.82419157413667     0.00      4320.0        360.00",
                      (2, 5L, 34.2682, 348.7242, 0.1859667, 331.7664, 19.3264, 10.82419157, 413667L, 0.00, 4320.0, 360.00).ToString(),
                      caster2
                   |]
        
        data
        |> Array.iter (fun (format, input, expected, caster) -> let result = sscanf.sscanf format input
                                                                let actual = caster result
                                                                actual |> should equal expected)
                                                             