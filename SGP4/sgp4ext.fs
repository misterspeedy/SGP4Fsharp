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
        // TODO this branch is not tested using the provided tests dataset (and parameters 'a', 'v' and '72')
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

// TODO rename

type Rv2CoeResult = 
        {
           p : double
           a : double
           ecc : double
           incl : double
           node : double
           argp : double
           nu : double
           m : double
           arglat : double
           truelon : double
           lonper : double
        }

let rv2coe (r : array<double>)
           (v : array<double>)
           (mu : double) =

    let getLongitude (bar0 : double) (vecMag : double) = 
        let mutable temp = bar0 / vecMag
        if ( Math.Abs(temp) > 1.0 ) then
            temp <- sgn(temp)
        acos(temp)

    let getOrbitType  (ecc : double) (incl : double) = 
        // TODO Only the EllipticalInclined branch is tested using the provided tests dataset (and parameters 'a', 'v' and '72')
        if ( ecc < small ) then
            // Circular equatorial:
            if  ((incl<small) || (Math.Abs(incl-PI)<small)) then
                OrbitType.CircularEquatorial
            else
                // Circular inclined:
                OrbitType.CircularInclined
        else
            // Elliptical, parabolic, hyperbolic equatorial:
            if  ((incl<small) || (Math.Abs(incl-PI)<small)) then
                OrbitType.EllipticalParabolicHyperbolicEquatorial
            else
                OrbitType.EllipticalInclined

    let magr = mag r
    let magv2 = (mag v) ** 2.0

    // Find h n and e vectors:
    let hbar = cross r v
    let magh = mag hbar

    if ( magh > small ) then
        let nbar = [| -hbar.[1]; hbar.[0]; 0.0 |]
        let magn = mag nbar
        let c1 = magv2 - mu /magr
        let rdotv = dot r v
        let ebar = [|0..2|] |> Array.map (fun i -> (c1*r.[i] - rdotv*v.[i])/mu)
        let ecc = mag ebar

        // Find a e and semi-latus rectum:
        let sme =( magv2*0.5  ) - ( mu /magr )

        let a =
            if ( Math.Abs( sme ) > small ) then
                -mu  / (2.0 *sme)
            else
                infinite

        let p = magh*magh/mu

        // Find inclination:
        let incl = hbar.[2]/magh |> acos

        // Determine type of orbit for later use:
        //   elliptical, parabolic, hyperbolic inclined
        let typeorbit = getOrbitType ecc incl

        // TODO these three seem to do similar things regarding < 0.0 and twopi - refactor

        // Find longitude of ascending node:
        let node = 
            if ( magn > small ) then
                let long = getLongitude nbar.[0] magn
                if ( nbar.[1] < 0.0 ) then
                    twopi - long
                else
                    long
            else
                undefined

        // Find argument of perigee:
        let argp = 
            if ( typeorbit = OrbitType.EllipticalInclined ) then
                let ang = angle nbar ebar
                if ( ebar.[2] < 0.0 ) then
                    twopi - ang
                else
                    ang
            else
                undefined

        // Find true anomaly at epoch:
        let nu = 
            if ( typeorbit = OrbitType.EllipticalInclined || typeorbit = OrbitType.EllipticalParabolicHyperbolicEquatorial ) then
                let ang = angle ebar r
                if (rdotv < 0.0) then
                    twopi - ang
                else
                    ang
            else
                undefined

        // Find argument of latitude - circular inclined:
        let arglat = 
            if ( typeorbit = OrbitType.CircularInclined ) then
                let ang = angle nbar r
                if ( r.[2] < 0.0 ) then
                    twopi - ang
                else
                    ang
            else
                undefined

        // Find longitude of perigee - elliptical equatorial:
        let lonper = 
            if  (( ecc>small ) && ( typeorbit = OrbitType.EllipticalParabolicHyperbolicEquatorial)) then
                // TODO this branch is not tested using the provided tests dataset (and parameters 'a', 'v' and '72' 
                let long = getLongitude ebar.[0] ecc
                if ( ebar.[1] < 0.0 ) || ( incl > halfpi ) then
                    twopi - long
                else
                    long
            else
                undefined

        // Find true longitude - circular equatorial:
        let truelon = 
            if  (( magr>small ) && ( typeorbit = OrbitType.CircularEquatorial)) then
                // TODO this branch is not tested using the provided tests dataset (and parameters 'a', 'v' and '72'
                let long = getLongitude r.[0] magr
                if ( r.[1] < 0.0  ) || ( incl > halfpi ) then
                    twopi - long
                else 
                    long
            else
                undefined

        // Find mean anomaly for all orbits:
        let m = 
            if (typeorbit = OrbitType.CircularInclined) && (arglat <> undefined) then 
                arglat
            elif (typeorbit = OrbitType.CircularEquatorial) && (magr>small) then
                truelon
            elif (typeorbit = OrbitType.EllipticalInclined) || (typeorbit = OrbitType.EllipticalParabolicHyperbolicEquatorial) then
                let mutable m' = Double.NaN
                let mutable e = Double.NaN // e is unused
                newtonnu ecc nu &e &m'
                m'
            else 
                infinite

        {
           p = p
           a = a
           ecc = ecc
           incl = incl
           node = node
           argp = argp
           nu = nu
           m = m
           arglat = arglat
           truelon = truelon
           lonper = lonper
        }

    else
        {
           p = undefined
           a = undefined
           ecc = undefined
           incl = undefined
           node = undefined
           argp = undefined
           nu = undefined
           m = undefined
           arglat = undefined
           truelon = undefined
           lonper = undefined
        }

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
