module testfsharp

open System
open Sgp4Constants
open sgp4common
open sscanf
open sgp4unit
open sgp4io
open sgp4ext

// TODO move to a utils module:
let Pair sequence =
    let a = Array.ofSeq sequence
    if (a.Length % 2) <> 0 then
        failwith "Can't pair - uneven length"
    else
        seq {
                for i in [0..2..a.Length-1] do
                    yield [a.[i]; a.[i+1]]
            }

let runTests dataDir = 
    let mutable str = Array.zeroCreate 2

    let mutable typerun = ' '
    let mutable typeinput = ' '
    let mutable opsmode = ' '
    let mutable whichconst = GravConstType.Wgs72

    let mutable whichcon = Int32.MinValue

    // TODO get rid of some of these - subsumed in coe record

    let mutable jd             = Double.NaN
    let mutable tsince         = Double.NaN
    let mutable startmfe       = Double.NaN
    let mutable stopmfe        = Double.NaN
    let mutable deltamin       = Double.NaN

    let satrec : ElSetRec = 
        {
            satnum        = Int32.MinValue 
            epochyr       = Int16.MinValue
            epochtynumrev = Int16.MinValue
            error         = Int16.MinValue
            operationmode = ' '
            init          = ' '
            method'       = ' '
            isimp         = Int16.MinValue
            aycof         = Double.NaN
            con41         = Double.NaN
            cc1           = Double.NaN
            cc4           = Double.NaN
            cc5           = Double.NaN
            d2            = Double.NaN
            d3            = Double.NaN
            d4            = Double.NaN
            delmo         = Double.NaN
            eta           = Double.NaN
            argpdot       = Double.NaN
            omgcof        = Double.NaN
            sinmao        = Double.NaN
            t             = Double.NaN
            t2cof         = Double.NaN
            t3cof         = Double.NaN
            t4cof         = Double.NaN
            t5cof         = Double.NaN
            x1mth2        = Double.NaN
            x7thm1        = Double.NaN
            mdot          = Double.NaN
            nodedot       = Double.NaN
            xlcof         = Double.NaN
            xmcof         = Double.NaN
            nodecf        = Double.NaN
            irez          = Int16.MinValue
            d2201         = Double.NaN
            d2211         = Double.NaN
            d3210         = Double.NaN
            d3222         = Double.NaN
            d4410         = Double.NaN
            d4422         = Double.NaN
            d5220         = Double.NaN
            d5232         = Double.NaN
            d5421         = Double.NaN
            d5433         = Double.NaN
            dedt          = Double.NaN
            del1          = Double.NaN
            del2          = Double.NaN
            del3          = Double.NaN
            didt          = Double.NaN
            dmdt          = Double.NaN
            dnodt         = Double.NaN
            domdt         = Double.NaN
            e3            = Double.NaN
            ee2           = Double.NaN
            peo           = Double.NaN
            pgho          = Double.NaN
            pho           = Double.NaN
            pinco         = Double.NaN
            plo           = Double.NaN
            se2           = Double.NaN
            se3           = Double.NaN
            sgh2          = Double.NaN
            sgh3          = Double.NaN
            sgh4          = Double.NaN
            sh2           = Double.NaN
            sh3           = Double.NaN
            si2           = Double.NaN
            si3           = Double.NaN
            sl2           = Double.NaN
            sl3           = Double.NaN
            sl4           = Double.NaN
            gsto          = Double.NaN
            xfact         = Double.NaN
            xgh2          = Double.NaN
            xgh3          = Double.NaN
            xgh4          = Double.NaN
            xh2           = Double.NaN
            xh3           = Double.NaN
            xi2           = Double.NaN
            xi3           = Double.NaN
            xl2           = Double.NaN
            xl3           = Double.NaN
            xl4           = Double.NaN
            xlamo         = Double.NaN
            zmol          = Double.NaN
            zmos          = Double.NaN
            atime         = Double.NaN
            xli           = Double.NaN
            xni           = Double.NaN
            a             = Double.NaN
            altp          = Double.NaN
            alta          = Double.NaN
            epochdays     = Double.NaN
            jdsatepoch    = Double.NaN
            nddot         = Double.NaN
            ndot          = Double.NaN
            bstar         = Double.NaN
            rcse          = Double.NaN
            inclo         = Double.NaN
            nodeo         = Double.NaN
            ecco          = Double.NaN
            argpo         = Double.NaN
            mo            = Double.NaN
            no            = Double.NaN
        }

    let monstr = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

    printfn "%s" SGP4Version

//    printfn("input operation mode a, i")
//    opsmode <- Console.ReadLine().[0]
//    printf("input type of run c, v, m")
//    typerun <- Console.ReadLine().[0]

    opsmode <- 'a'
    typerun <- 'v'

    if ((typerun <> 'v') && (typerun <> 'c')) then
        printfn("input mfe, epoch (YMDHMS), or dayofyr approach, m,e,d")
        typeinput <-Console.ReadLine().[0]
    else
        typeinput <- 'e'

//    printfn("input which constants 721 72 84 ");
//    whichcon <- Console.ReadLine() |> Int32.Parse
//    if (whichcon = 721) then whichconst <- Wgs72old
//    if (whichcon = 72)  then whichconst <- Wgs72
//    if (whichcon = 84)  then whichconst <- Wgs84

    whichconst <- Wgs72

    let gravConsts = getgravconst whichconst 

//    printfn "input elset filename:"
//    let infilename = Console.ReadLine()
    //let infilename = @"C:\Code\VS2012\SGP4\FSharpVersion\SGP4\SGP4\obj\Debug\SGP4-VER_single.TLE"
    let infilename = IO.Path.Combine(dataDir, "SGP4-VER.TLE")
    // let infilename = @"C:\Code\VS2012\SGP4\FSharpVersion\SGP4\SGP4\obj\Debug\04632.TLE"

    let fileLines = System.IO.File.ReadAllLines(infilename)
    // TODO return 1 on exception             
    // printf("Failed to open file: %s\n", infilename);

    let outFileLines = new ResizeArray<string>()

    let fileLinePairs = fileLines 
                        |> Array.filter (fun line -> line.StartsWith("#") |> not)
                        |> Pair

    for pair in fileLinePairs do
        let longstr1 = pair.[0].ToCharArray()
        let longstr2 = pair.[1].ToCharArray()

        twoline2rv longstr1 longstr2 typerun typeinput opsmode whichconst 
                &startmfe &stopmfe &deltamin satrec |> ignore
        outFileLines.Add(sprintf "%i xx" satrec.satnum)
        printfn "%i" satrec.satnum
        // call the propagator to get the initial state vector value
        let ok, ro, vo = sgp4 whichconst satrec 0.0

        if ok then
            let ro, vo = ro.Value, vo.Value

            // Generate .e files for stk
            jd <- satrec.jdsatepoch
            let eName = new String(longstr1.[2..6]) + ".e"
            let eLines = new ResizeArray<string>()
            let ijd = invjday jd 
            eLines.Add("stk.v.4.3 ") // must use 4.3...

            eLines.Add("")
            eLines.Add("BEGIN Ephemeris ")
            eLines.Add(" ")
            eLines.Add("NumberOfEphemerisPoints		146 ")
            eLines.Add(sprintf "ScenarioEpoch	  %3i %3s%5i%3i:%2i:%12.9f " ijd.day monstr.[ijd.mon-1] ijd.year ijd.hr ijd.minute ijd.sec) // F# fixed bug - mon range is 1..12 so out of index range
            eLines.Add("InterpolationMethod		Lagrange ")
            eLines.Add("InterpolationOrder		5 ")
            eLines.Add("CentralBody				Earth ")
            eLines.Add("CoordinateSystem			TEME ")
            eLines.Add(sprintf "CoordinateSystemEpoch	%3i %3s%5i%3i:%2i:%12.9f " ijd.day monstr.[ijd.mon-1] ijd.year ijd.hr ijd.minute ijd.sec) // F# fixed bug - mon range is 1..12 so out of index range
            eLines.Add("DistanceUnit			Kilometers ")
            eLines.Add(" ")
            eLines.Add("EphemerisTimePosVel ")
            eLines.Add(" ")
            eLines.Add(sprintf " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f" satrec.t ro.[0] ro.[1] ro.[2] vo.[0] vo.[1] vo.[2])
            outFileLines.Add(sprintf " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f" satrec.t ro.[0] ro.[1] ro.[2] vo.[0] vo.[1] vo.[2])

            tsince <- startmfe
            // check so the first value isn't written twice
            if ( Math.Abs(tsince) > 1.0e-8 ) then
                tsince <- tsince - deltamin

            // Loop to perform the propagation:
            while ((tsince < stopmfe) && (satrec.error = 0s)) do
                tsince <- tsince + deltamin

                if (tsince > stopmfe) then
                    tsince <- stopmfe

                let ok, ro, vo = sgp4 whichconst satrec tsince 

                if ok then
                    let ro, vo = ro.Value, vo.Value

                    if (satrec.error > 0s) then
                        printfn "# *** error: t:= %f *** code = %3d" satrec.t satrec.error

                    if (satrec.error = 0s) then
                        if ((typerun <> 'v') && (typerun <> 'c')) then
                            jd <- satrec.jdsatepoch + tsince/minperday
                            let ijd = invjday jd  
                            outFileLines.Add(sprintf " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f %5i%3i%3i %2i:%2i:%9.6f"
                                                       tsince ro.[0] ro.[1] ro.[2] vo.[0] vo.[1] vo.[2] ijd.year ijd.mon ijd.day ijd.hr ijd.minute ijd.sec )
                        else
                            jd <- satrec.jdsatepoch + tsince/minperday
                            let ijd = invjday jd

                            eLines.Add(sprintf " %16.6f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f "
                                                       (tsince*60.0) ro.[0] ro.[1] ro.[2] vo.[0] vo.[1] vo.[2])

                            let outLinePart1 = sprintf " %16.8f %16.8f %16.8f %16.8f %12.9f %12.9f %12.9f" 
                                                       tsince ro.[0] ro.[1] ro.[2] vo.[0] vo.[1] vo.[2]

                            let coe = rv2coe ro vo gravConsts.mu

                            let outLinePart2 = sprintf " %14.6f %8.6f %10.5f %10.5f %10.5f %10.5f %10.5f %5i%3i%3i %2i:%2i:%9.6f"
                                                        coe.a coe.ecc (coe.incl*rad) (coe.node*rad) (coe.argp*rad) (coe.nu*rad) (coe.m*rad) ijd.year ijd.mon ijd.day ijd.hr ijd.minute ijd.sec

                            outFileLines.Add(outLinePart1 + outLinePart2)
                else
                    // TODO handle not OK from sgp4 call
                    ()

            eLines.Add(" END Ephemeris ")
            IO.File.WriteAllLines(IO.Path.Combine(dataDir, eName), eLines)
        else
            // TODO handle not OK from sgp4 call
            ()
    if (typerun = 'c') then
        System.IO.File.WriteAllLines(IO.Path.Combine(dataDir, "tfsharpall.out"), outFileLines)
    else if (typerun = 'v') then
        System.IO.File.WriteAllLines(IO.Path.Combine(dataDir, "tfsharpver.out"), outFileLines)
    else
        System.IO.File.WriteAllLines(IO.Path.Combine(dataDir, "tsharp.out"), outFileLines)
