module sgp4io

open System
open Sgp4Constants
open sgp4unit
open sgp4common
open sscanf
open sgp4ext

let twoline2rv (longstr1 : array<char>)
               (longstr2 : array<char>)
               (typerun : char)
               (typeinput : char)
               (opsmode : char)
               (whichconst : GravConstType)
               (startmfe : double byref)
               (stopmfe : double byref)
               (deltamin : double byref) 
               (satrec : ElSetRec) = 
    let xpdotp  = 1440.0 / (2.0 * PI)  // 229.1831180523293

    let mutable startsec      = Double.NaN
    let mutable stopsec       = Double.NaN
    let mutable startdayofyr  = Double.NaN
    let mutable stopdayofyr   = Double.NaN

    let mutable startyear = Int32.MinValue
    let mutable stopyear  = Int32.MinValue
    let mutable startmon  = Int32.MinValue
    let mutable stopmon   = Int32.MinValue
    let mutable startday  = Int32.MinValue
    let mutable stopday   = Int32.MinValue
    let mutable starthr   = Int32.MinValue
    let mutable stophr    = Int32.MinValue
    let mutable startmin  = Int32.MinValue
    let mutable stopmin   = Int32.MinValue
    let mutable cardnumb  = Int32.MinValue
    let mutable numb      = Int32.MinValue
    let mutable j         = Int32.MinValue

    let mutable revnum = 0L
    let mutable elnum = 0L

    let mutable classification = ' '
    let mutable intldesg = [||]

    let mutable year = 0

    let mutable nexp   = Int32.MinValue
    let mutable ibexp  = Int32.MinValue

    let gravConsts = getgravconst whichconst 
    
    //&tumin &mu &radiusearthkm &xke &j2 &j3 &j4 &j3oj2

    satrec.error <- 0s

    // Set the implied decimal points since doing a formated read
    // fixes for bad input data values (missing, ...):
    for j in [10..15] do
        if (longstr1.[j] = ' ') then
            longstr1.[j] <- '_'

    if (longstr1.[44] <> ' ') then
        longstr1.[43] <- longstr1.[44]
    longstr1.[44] <- '.'
    if (longstr1.[7] = ' ') then
        longstr1.[7] <- 'U'
    if (longstr1.[9] = ' ') then
        longstr1.[9] <- '.'
    for j in [45..49] do
        if (longstr1.[j] = ' ') then
            longstr1.[j] <- '0'
    if (longstr1.[51] = ' ') then
        longstr1.[51] <- '0'
    if (longstr1.[53] <> ' ') then
        longstr1.[52] <- longstr1.[53]
    longstr1.[53] <- '.'
    longstr2.[25] <- '.'
    for j in [26..32] do
        if (longstr2.[j] = ' ') then
            longstr2.[j] <- '0'
    if (longstr1.[62] = ' ') then
        longstr1.[62] <- '0'
    if (longstr1.[68] = ' ') then
        longstr1.[68] <- '0'

    let sscanf1 = sscanf "%2d %5ld %1c %10s %2d %12lf %11lf %7lf %2d %7lf %2d %2d %6ld" (new String(longstr1))

    cardnumb         <- sscanf1.[0]  :?> int
    satrec.satnum    <- sscanf1.[1]  :?> int64 |> int
    classification   <- sscanf1.[2]  :?> char
    intldesg         <- (sscanf1.[3]  :?> string).ToCharArray()
    satrec.epochyr   <- sscanf1.[4]  :?> int |> int16
    satrec.epochdays <- sscanf1.[5]  :?> double
    satrec.ndot      <- sscanf1.[6]  :?> double
    satrec.nddot     <- sscanf1.[7]  :?> double
    nexp             <- sscanf1.[8]  :?> int
    satrec.bstar     <- sscanf1.[9]  :?> double
    ibexp            <- sscanf1.[10] :?> int
    numb             <- sscanf1.[11] :?> int
    elnum            <- sscanf1.[12] :?> int64

    // Run for specified times from the file:
    if (typerun = 'v') then 
        if (longstr2.[52] = ' ') then
            //let sscanf2 = sscanf "%2d %5ld %9lf %9lf %8lf %9lf %9lf %10lf %6ld %lf %lf %lf" (new String(longstr2))
            let str = new String(longstr2)
            let sscanf2 = sscanf "%2d %5ld %9lf %9lf %8lf %9lf %9lf %12lf %6ld %lf %lf %lf" str // 5th from last changed from 11 to 12
            cardnumb      <- sscanf2.[0]  :?> int
            satrec.satnum <- sscanf2.[1]  :?> int64 |> int
            satrec.inclo  <- sscanf2.[2]  :?> double
            satrec.nodeo  <- sscanf2.[3]  :?> double
            satrec.ecco   <- sscanf2.[4]  :?> double
            satrec.argpo  <- sscanf2.[5]  :?> double
            satrec.mo     <- sscanf2.[6]  :?> double
            satrec.no     <- sscanf2.[7]  :?> double
            revnum        <- sscanf2.[8]  :?> int64
            startmfe      <- sscanf2.[9]  :?> double
            stopmfe       <- sscanf2.[10] :?> double
            deltamin      <- sscanf2.[11] :?> double
        else
            let str =  (new String(longstr2))
            let sscanf2 = sscanf "%2d %5ld %9lf %9lf %8lf %9lf %9lf %12lf %6ld %lf %lf %lf" str // 5th from last changed from 10 to 12
            cardnumb      <- sscanf2.[0]  :?> int
            satrec.satnum <- sscanf2.[1]  :?> int64 |> int
            satrec.inclo  <- sscanf2.[2]  :?> double
            satrec.nodeo  <- sscanf2.[3]  :?> double
            satrec.ecco   <- sscanf2.[4]  :?> double
            satrec.argpo  <- sscanf2.[5]  :?> double
            satrec.mo     <- sscanf2.[6]  :?> double
            satrec.no     <- sscanf2.[7]  :?> double
            revnum        <- sscanf2.[8]  :?> int64
            startmfe      <- sscanf2.[9]  :?> double
            stopmfe       <- sscanf2.[10] :?> double
            deltamin      <- sscanf2.[11] :?> double

    else // Simply run -1 day to +1 day or user input times
        if (longstr2.[52] = ' ') then
            let sscanf2 = sscanf "%2d %5ld %9lf %9lf %8lf %9lf %9lf %12lf %6ld " (new String(longstr2)) // 2nd from last changed from 10 to 12
            cardnumb      <- sscanf2.[0]  :?> int
            satrec.satnum <- sscanf2.[1]  :?> int64 |> int
            satrec.inclo  <- sscanf2.[2]  :?> double
            satrec.nodeo  <- sscanf2.[3]  :?> double
            satrec.ecco   <- sscanf2.[4]  :?> double
            satrec.argpo  <- sscanf2.[5]  :?> double
            satrec.mo     <- sscanf2.[6]  :?> double
            satrec.no     <- sscanf2.[7]  :?> double
            revnum        <- sscanf2.[8]  :?> int64
        else
            let sscanf2 = sscanf "%2d %5ld %9lf %9lf %8lf %9lf %9lf %12lf %6ld \n" (new String(longstr2)) // 2nd from last changed from 10 to 12
            cardnumb      <- sscanf2.[0]  :?> int
            satrec.satnum <- sscanf2.[1]  :?> int64 |> int
            satrec.inclo  <- sscanf2.[2]  :?> double
            satrec.nodeo  <- sscanf2.[3]  :?> double
            satrec.ecco   <- sscanf2.[4]  :?> double
            satrec.argpo  <- sscanf2.[5]  :?> double
            satrec.mo     <- sscanf2.[6]  :?> double
            satrec.no     <- sscanf2.[7]  :?> double
            revnum        <- sscanf2.[8]  :?> int64

    // Find no, ndot, nddot:
    satrec.no    <- satrec.no / xpdotp //* rad/min
    satrec.nddot <- satrec.nddot * Math.Pow(10.0, float(nexp))
    satrec.bstar <- satrec.bstar * Math.Pow(10.0, float(ibexp))

    // Convert to sgp4 units:
    satrec.a     <- Math.Pow( satrec.no*gravConsts.tumin , (-2.0/3.0) )
    satrec.ndot  <- satrec.ndot  / (xpdotp*1440.0)  //* ? * minperday
    satrec.nddot <- satrec.nddot / (xpdotp*1440.0*1440.0)

    // Find standard orbital elements:
    satrec.inclo <- satrec.inclo  * deg2rad
    satrec.nodeo <- satrec.nodeo  * deg2rad
    satrec.argpo <- satrec.argpo  * deg2rad
    satrec.mo    <- satrec.mo     * deg2rad

    satrec.alta <- satrec.a*(1.0 + satrec.ecco) - 1.0
    satrec.altp <- satrec.a*(1.0 - satrec.ecco) - 1.0

    // Find sgp4epoch time of element set
    // remember that sgp4 uses units of days from 0 jan 1950 (sgp4epoch)
    // and minutes from the epoch (time)

    // Temp fix for years from 1957-2056 
    // Correct fix will occur when year is 4-digit in tle:
    if (satrec.epochyr < 57s) then
        year <- int(satrec.epochyr) + 2000
    else
        year <- int(satrec.epochyr) + 1900

    let ymdhms = days2mdhms year satrec.epochdays
    satrec.jdsatepoch <- jday ymdhms 

    // Input start stop times manually
    if ((typerun <> 'v') && (typerun <> 'c')) then
        // Enter start/stop ymd hms values:
        if (typeinput = 'e') then
            printf("input start prop year mon day hr min sec \n")
            // Make sure there is no space at the end of the format specifiers in scanf!
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%i %i %i %i %i %lf" scanLine
            let startymdhms = 
                {
                     year   = ssscanf.[0] :?> int
                     mon    = ssscanf.[1] :?> int
                     day    = ssscanf.[2] :?> int
                     hr     = ssscanf.[3] :?> int
                     minute = ssscanf.[4] :?> int
                     sec    = ssscanf.[5] :?> double
                }
            let jdstart = jday startymdhms

            printf("input stop prop year mon day hr min sec \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%i %i %i %i %i %lf" scanLine
            let stopymdhms = 
                {
                     year   = ssscanf.[0] :?> int
                     mon    = ssscanf.[1] :?> int
                     day    = ssscanf.[2] :?> int
                     hr     = ssscanf.[3] :?> int
                     minute = ssscanf.[4] :?> int
                     sec    = ssscanf.[5] :?> double
                }
            let jdstop = jday stopymdhms

            startmfe <- (jdstart - satrec.jdsatepoch) * 1440.0
            stopmfe  <- (jdstop - satrec.jdsatepoch) * 1440.0

            printf("input time step in minutes \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%lf" scanLine
            deltamin <- ssscanf.[0] :?> double
        // Enter start/stop year and days of year values:
        if (typeinput = 'd') then
            printf("input start year dayofyr \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%i %lf" scanLine
            let startyear = ssscanf.[0] :?> int
            let startdayofyr = ssscanf.[1] :?> double
            printf("input stop year dayofyr \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%i %lf" scanLine
            let stopyear = ssscanf.[0] :?> int
            let topdayofyr = ssscanf.[1] :?> int

            let startymdhms = days2mdhms startyear startdayofyr
            let jdstart = jday startymdhms
            let stopymdhms = days2mdhms stopyear stopdayofyr
            let jdstop = jday stopymdhms

            startmfe <- (jdstart - satrec.jdsatepoch) * 1440.0
            stopmfe  <- (jdstop - satrec.jdsatepoch) * 1440.0

            printf("input time step in minutes \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%lf" scanLine
            deltamin <- ssscanf.[0] :?> double
        // Enter start/stop mfe values:
        if (typeinput = 'm') then
            printf("input start min from epoch \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%lf" scanLine
            startmfe <- ssscanf.[0] :?> double
            printf("input stop min from epoch \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%lf" scanLine
            stopmfe <- ssscanf.[0] :?> double
            printf("input time step in minutes \n")
            let scanLine = Console.ReadLine()
            let ssscanf = sscanf "%lf" scanLine
            deltamin <- ssscanf.[0] :?> double

    // Perform complete catalog evaluation, -+ 1 day:
    if (typerun = 'c') then
        startmfe <- -1440.0
        stopmfe  <-  1440.0
        deltamin <-    10.0

    // Initialize the orbit at sgp4epoch:
    sgp4init whichconst opsmode (int16(satrec.satnum)) (satrec.jdsatepoch-2433281.5) satrec.bstar
        satrec.ecco satrec.argpo satrec.inclo satrec.mo satrec.no 
        satrec.nodeo satrec
