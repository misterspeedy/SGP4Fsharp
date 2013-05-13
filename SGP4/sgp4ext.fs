module sgp4ext

open System
open Sgp4Constants
open Sgp4Math
open sgp4common

let angle (vec1 : array<double>) (vec2 : array<double>) =
    let magv1v2 = mag vec1 * mag vec2

    if (magv1v2 > small**2.0) then
        let mutable temp = (dot vec1 vec2) / magv1v2
        if Math.Abs(temp) > 1.0 then
            temp <- sgn temp
        acos(temp)
    else
        undefined

let asinh xval =
    log (xval + sqrt(xval * xval + 1.0))

let newtonnu (ecc : double) (nu : double) (e0 : double byref) (m : double byref) =
    let mutable sine = Double.NaN
    let mutable cose = Double.NaN

    e0 <- 999999.9
    m <- 999999.9 

    if (Math.Abs(ecc) < small) then
        m <- nu
        e0 <- nu
    else
        // Elliptical:
        if (ecc < 1.0 - small) then
             sine <- ( sqrt( 1.0 - ecc*ecc ) * sin(nu) ) / ( 1.0 + ecc * cos(nu) )
             cose <- ( ecc + cos(nu) ) / ( 1.0  + ecc*cos(nu) )
             e0   <- atan2 sine cose
             m    <- e0 - ecc * sin(e0)
        // Hyperbolic:
        else
            if ( ecc > 1.0 + small ) then
                if ((ecc > 1.0 ) && (Math.Abs(nu)+0.00001 < PI-acos(1.0 /ecc))) then
                    sine <- ( sqrt( ecc*ecc-1.0  ) * sin(nu) ) / ( 1.0  + ecc*cos(nu) )
                    e0   <- asinh( sine )
                    m    <- ecc*sinh(e0) - e0
            else
                // Parabolic:
                if ( Math.Abs(nu) < 168.0*PI/180.0  ) then
                    e0 <- tan( nu*0.5 )
                    m  <- e0 + (e0*e0*e0)/3.0

    if ( ecc < 1.0  ) then
        m <- fmod m (2.0 * PI)
        if ( m < 0.0  ) then
            m <- m + 2.0 * PI
        e0 <- fmod e0 (2.0 * PI)

let rv2coe (r : array<double>)
           (v : array<double>)
           (mu : double)
           (p : double byref)
           (a : double byref)
           (ecc : double byref)
           (incl : double byref)
           (omega : double byref)
           (argp : double byref)
           (nu : double byref)
           (m : double byref)
           (arglat : double byref)
           (truelon : double byref)
           (lonper : double byref) =

    let nbar             = [|Double.NaN; Double.NaN; Double.NaN|]
    let mutable magr     = Double.NaN
    let mutable magv     = Double.NaN
    let mutable magn     = Double.NaN
    let ebar             = [|Double.NaN; Double.NaN; Double.NaN|]
    let mutable sme      = Double.NaN
    let mutable rdotv    = Double.NaN
    let mutable infinite = Double.NaN
    let mutable temp     = Double.NaN
    let mutable c1       = Double.NaN
    let mutable hk       = Double.NaN
    let mutable magh     = Double.NaN
    let mutable e        = Double.NaN

    let mutable typeorbit = OrbitType.Undefined

    magr <- mag r
    magv <- mag v

    // Find h n and e vectors:
    let hbar = cross r v
    magh <- mag hbar
    if ( magh > small ) then
        nbar.[0] <- -hbar.[1]
        nbar.[1] <-  hbar.[0]
        nbar.[2] <-   0.0
        magn <- mag nbar
        c1 <- magv*magv - mu /magr
        rdotv <- dot r v
        for i in [0..2] do
            ebar.[i] <- (c1*r.[i] - rdotv*v.[i])/mu
        ecc <- mag ebar

        // Find a e and semi-latus rectum:
        sme <- ( magv*magv*0.5  ) - ( mu /magr )
        if ( Math.Abs( sme ) > small ) then
            a <- -mu  / (2.0 *sme)
        else
            a <- infinite
        p <- magh*magh/mu

        // Find inclination:
        hk <- hbar.[2]/magh
        incl <- acos(hk)

        // Determine type of orbit for later use:
        //   elliptical, parabolic, hyperbolic inclined
        typeorbit <- OrbitType.EllipticalInclined
        if ( ecc < small ) then
            // Circular equatorial:
            if  ((incl<small) || (Math.Abs(incl-PI)<small)) then
                typeorbit <- OrbitType.CircularEquatorial
            else
                // Circular inclined:
                typeorbit <- OrbitType.CircularInclined
        else
            // Elliptical, parabolic, hyperbolic equatorial:
            if  ((incl<small) || (Math.Abs(incl-PI)<small)) then
                typeorbit <- OrbitType.EllipticalParabolicHyperbolicEquatorial

        // Find longitude of ascending node:
        if ( magn > small ) then
            temp <- nbar.[0] / magn
            if ( Math.Abs(temp) > 1.0  ) then
                temp <- sgn(temp)
            omega <- acos(temp)
            if ( nbar.[1] < 0.0  ) then
                omega <- twopi - omega
        else
            omega <- undefined

        // Find argument of perigee:
        if ( typeorbit = OrbitType.EllipticalInclined ) then
            argp <- angle nbar ebar
            if ( ebar.[2] < 0.0  ) then
                argp <- twopi - argp
        else
            argp <- undefined

        // Find true anomaly at epoch:
        if ( typeorbit = OrbitType.EllipticalInclined || typeorbit = OrbitType.EllipticalParabolicHyperbolicEquatorial ) then
            nu <- angle ebar r
            if (rdotv < 0.0) then
                nu <- twopi - nu
        else
            nu <- undefined

        // Find argument of latitude - circular inclined:
        if ( typeorbit = OrbitType.CircularInclined ) then
            arglat <- angle nbar r
            if ( r.[2] < 0.0  ) then
                arglat <- twopi - arglat
            m <- arglat
        else
            arglat <- undefined

        // Find longitude of perigee - elliptical equatorial:
        if  (( ecc>small ) && ( typeorbit = OrbitType.EllipticalParabolicHyperbolicEquatorial)) then
            temp <- ebar.[0]/ecc
            if ( Math.Abs(temp) > 1.0  ) then
                temp <- sgn(temp)
            lonper <- acos( temp )
            if ( ebar.[1] < 0.0  ) then
                lonper <- twopi - lonper
            if ( incl > halfpi ) then
                lonper <- twopi - lonper
        else
            lonper <- undefined

        // Find true longitude - circular equatorial:
        if  (( magr>small ) && ( typeorbit = OrbitType.CircularEquatorial)) then
            temp <- r.[0]/magr
            if ( Math.Abs(temp) > 1.0  ) then
                temp <- sgn(temp)
            truelon <- acos( temp )
            if ( r.[1] < 0.0  ) then
                truelon <- twopi - truelon
            if ( incl > halfpi ) then
                truelon <- twopi - truelon
            m <- truelon
        else
            truelon <- undefined

        // Find mean anomaly for all orbits:
        if ( typeorbit = OrbitType.EllipticalInclined || typeorbit = OrbitType.EllipticalParabolicHyperbolicEquatorial ) then
            newtonnu ecc nu &e &m
    else
        p       <- undefined
        a       <- undefined
        ecc     <- undefined
        incl    <- undefined
        omega   <- undefined
        argp    <- undefined
        nu      <- undefined
        m       <- undefined
        arglat  <- undefined
        truelon <- undefined
        lonper  <- undefined

let jday (year : int)
         (mon : int)
         (day : int)
         (hr : int)
         (minute : int)
         (sec : double)
         (jd : double byref) =
    let doubleyear = double(year)
    let doublemon = double(mon)
    let doubleday = double(day)
    let doublehr = double(hr)
    let doubleminute = double(minute)
    
    jd <- 367.0 * doubleyear -
          floor((7.0 * (doubleyear + floor((doublemon + 9.0) / 12.0))) * 0.25) +
          floor( 275.0 * doublemon / 9.0 ) +
          doubleday + 1721013.5 +
          ((sec / 60.0 + doubleminute) / 60.0 + doublehr) / 24.0 // ut in days

let days2mdhms (year : int)
               (days : double)
               (mon : int byref)
               (day : int byref)
               (hr : int byref)
               (minute : int byref)
               (sec : double byref) =
    let mutable temp = Double.NaN

    let lmonth = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]

    let dayofyr = days |> floor |> int

    // Find month and day of month: 
    // TODO this is not correct for all leap years
    if ( (year % 4) = 0 ) then
        lmonth.[1] <- 29

    let mutable i = 1
    let mutable inttemp = 0
    while ((dayofyr > inttemp + lmonth.[i-1]) && (i < 12)) do
       inttemp <- inttemp + lmonth.[i-1]
       i <- i + 1
    mon <- i
    day <- dayofyr - inttemp

    // Find hours minutes and seconds
    temp   <- (days - double(dayofyr)) * 24.0
    hr     <- int(floor(temp))
    temp   <- (temp - float(hr)) * 60.0
    minute <- int(floor(temp))
    sec    <- (temp - float(minute)) * 60.0

let invjday (jd : double)
            (year : int byref)
            (mon : int byref)
            (day : int byref)
            (hr : int byref)
            (minute : int byref)
            (sec : double byref) =
    // Find year and days of the year:
    let temp = jd - 2415019.5
    let tu = temp / 365.25
    year <- 1900 + int(floor(tu))
    let mutable leapyrs = int(floor(double((year - 1901)) * 0.25))

    // Optional nudge by 8.64x10-7 sec to get even outputs:
    // days    = temp - ((year - 1900) * 365.0 + leapyrs) +            0.00000000001
    let mutable days = temp - float(((year - 1900) * 365 + leapyrs)) + 0.00000000001

    // Check for case of beginning of a year:
    if (days < 1.0) then
        year    <- year - 1
        leapyrs <- int(floor(double((year - 1901)) * 0.25))
        days    <- temp - (double((year - 1900) * 365) + double(leapyrs))

    // Find remaining data:
    days2mdhms year days &mon &day &hr &minute &sec
    sec <- sec - 0.00000086400
