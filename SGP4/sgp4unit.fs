module sgp4unit

open System
open Sgp4Constants
open Sgp4Math
open sgp4common

// From sgp4unit.h

// C++ math functions 

type GravityConstants = 
    {
        tumin : double 
        mu : double 
        radiusearthkm : double 
        xke : double 
        j2 : double 
        j3 : double 
        j4 : double 
        j3oj2 : double  
    }

let getgravconst (whichconst : GravConstType) =
    let mu = match whichconst with
             | Wgs72old -> 398600.79964 // in km3 / s2
             | Wgs72 ->    398600.8
             | Wgs84 ->    398600.5
    let radiusearthkm = match whichconst with
                        | Wgs72old   
                        | Wgs72 ->    6378.135 // km
                        | Wgs84 ->    6378.137
    let xke = match whichconst with
              | Wgs72old -> 0.0743669161 
              | Wgs72
              | Wgs84 ->    60.0 / sqrt(radiusearthkm**3.0/mu)
    let j2 = match whichconst with
             | Wgs72old 
             | Wgs72 ->    0.001082616
             | Wgs84 ->    0.00108262998905
    let j3 = match whichconst with
             | Wgs72old 
             | Wgs72 ->    -0.00000253881
             | Wgs84 ->    -0.00000253215306
    let j4 = match whichconst with
             | Wgs72old
             | Wgs72 ->    -0.00000165597
             | Wgs84 ->    -0.00000161098761

    let tumin  = 1.0 / xke
    let j3oj2  = j3 / j2   

    {
        mu     =   mu
        radiusearthkm = radiusearthkm
        xke    = xke
        j2     = j2
        j3     = j3
        j4     = j4
        tumin  = tumin
        j3oj2  = j3oj2
    }

let fixQuadrant x = 
    if x < 0.0 then
        x + twopi
    else
        x

let gstime (jdut1 : double) =
    let tut1 = (jdut1 - 2451545.0) / 36525.0

    fmod(( -6.2e-6 * tut1 * tut1 * tut1 + 0.093104 * (tut1**2.0) +
           (876600.0 * 3600.0 + 8640184.812866) * tut1 + 67310.54841) * deg2rad / 240.0) twopi 
    |> fixQuadrant

type InitlResult =
    {
          no: double
          method' : char
          ainv : double
          ao : double
          con41 : double
          con42 : double
          cosio: double
          cosio2 : double
          eccsq : double
          omeosq : double
          posq: double
          rp : double
          rteosq: double
          sinio  : double
          gsto : double
    }

let initl (satn : int)
          (whichconst: GravConstType)
          (ecco : double)
          (epoch : double)
          (inclo : double)
          (no : double)
          (opsmode: char) =

    // Earth constants:
    let gravConsts = getgravconst whichconst
    let x2o3 = 2.0 / 3.0

    // Calculate auxillary epoch quantities:
    let eccsq  = ecco * ecco
    let omeosq = 1.0 - eccsq
    let rteosq = sqrt(omeosq)
    let cosio  = cos(inclo)
    let cosio2 = cosio * cosio

    // Un-kozai the mean motion:
    let ak   = Math.Pow(gravConsts.xke / no, x2o3)
    let d1   = 0.75 * gravConsts.j2 * (3.0 * cosio2 - 1.0) / (rteosq * omeosq)
    let del  = d1 / (ak * ak)
    let adel = ak * (1.0 - del * del - del *
                   (1.0 / 3.0 + 134.0 * del * del / 81.0))
    let del = d1/(adel * adel)
    let no = no / (1.0 + del)
          
    let ao    = Math.Pow(gravConsts.xke / no, x2o3)
    let sinio = sin(inclo)
    let po = ao * omeosq
    let con42 = 1.0 - 5.0 * cosio2
    let con41 = -con42-cosio2-cosio2
    let ainv  = 1.0 / ao
    let posq  = po * po
    let rp    = ao * (1.0 - ecco)
    let method' = 'n'
     
    let gsto = 
        if (opsmode = 'a') then
            // Count integer number of days from 0 jan 1970
            let ts70 = epoch - 7305.0
            let ds70 = floor(ts70 + 1.0e-8)
            let tfrac = ts70 - ds70
            // Find greenwich location at epoch
            let c1     = 1.72027916940703639e-2
            let thgr70 = 1.7321343856509374
            let fk5r   = 5.07551419432269442e-15
            let c1p2p  = c1 + twopi
            fmod (thgr70 + c1*ds70 + c1p2p*tfrac + ts70*ts70*fk5r) twopi
            |> fixQuadrant
        else
            gstime(epoch + 2433281.5)

    {
          no = no
          method' = method'
          ainv = ainv
          ao = ao
          con41 = con41
          con42 = con42
          cosio = cosio
          cosio2 = cosio2
          eccsq = eccsq
          omeosq = omeosq
          posq = posq
          rp = rp
          rteosq = rteosq
          sinio = sinio
          gsto = gsto
    }

// Function declarations:

let dpper (e3      : double)
          (ee2     : double)
          (peo     : double)
          (pgho    : double)
          (pho     : double)
          (pinco   : double)
          (plo     : double)
          (se2     : double)
          (se3     : double)
          (sgh2    : double)
          (sgh3    : double)
          (sgh4    : double)
          (sh2     : double)
          (sh3     : double)
          (si2     : double)
          (si3     : double)
          (sl2     : double)
          (sl3     : double)
          (sl4     : double)
          (t       : double)
          (xgh2    : double)
          (xgh3    : double)
          (xgh4    : double)
          (xh2     : double)
          (xh3     : double)
          (xi2     : double)
          (xi3     : double)
          (xl2     : double)
          (xl3     : double)
          (xl4     : double)
          (zmol    : double)
          (zmos    : double)
          (inclo   : double)
          (init     : char)
          (ep      : double byref)
          (inclp   : double byref)
          (nodep   : double byref)
          (argpp   : double byref)
          (mp      : double byref) 
          (opsmode : char) =

    // Constants:
    let zns = 1.19459e-5
    let zes = 0.01675
    let znl = 1.5835218e-4
    let zel = 0.05490

    // Calculate time-varying periodics:
    // Be sure that the initial call has time set to zero:
    let mutable zm = if (init = 'y') then
                        zmos
                     else
                        zmos + zns * t
                  
    let zf    = zm + 2.0 * zes * sin(zm)
    let sinzf = sin(zf)
    let f2    =  0.5 * sinzf * sinzf - 0.25
    let f3    = -0.5 * sinzf * cos(zf)
    let ses   = se2* f2 + se3 * f3
    let sis   = si2 * f2 + si3 * f3
    let sls   = sl2 * f2 + sl3 * f3 + sl4 * sinzf
    let sghs  = sgh2 * f2 + sgh3 * f3 + sgh4 * sinzf
    let shs   = sh2 * f2 + sh3 * f3

    zm <- if (init = 'y') then
              zmol
          else
              zmol + znl * t
  
    let zf    = zm + 2.0 * zel * sin(zm)
    let sinzf = sin(zf)
    let f2    =  0.5 * sinzf * sinzf - 0.25
    let f3    = -0.5 * sinzf * cos(zf)
    let sel   = ee2 * f2 + e3 * f3
    let sil   = xi2 * f2 + xi3 * f3
    let sll   = xl2 * f2 + xl3 * f3 + xl4 * sinzf
    let sghl  = xgh2 * f2 + xgh3 * f3 + xgh4 * sinzf
    let shll  = xh2 * f2 + xh3 * f3
    let mutable pe    = ses + sel
    let mutable pinc  = sis + sil
    let mutable pl    = sls + sll
    let mutable pgh   = sghs + sghl
    let mutable ph    = shs + shll

    if (init = 'n') then
        pe    <- pe - peo
        pinc  <- pinc - pinco
        pl    <- pl - plo
        pgh   <- pgh - pgho
        ph    <- ph - pho
        inclp <- inclp + pinc
        ep    <- ep + pe
        let sinip = sin(inclp)
        let cosip = cos(inclp)

        if (inclp >= 0.2) then
            // Apply periodics directly:
            ph     <- ph / sinip
            pgh    <- pgh - cosip * ph
            argpp  <- argpp + pgh
            nodep  <- nodep + ph
            mp     <- mp + pl
        else
           // Apply periodics with Lyddane modification:
           let sinop  = sin(nodep)
           let cosop  = cos(nodep)
           let mutable alfdp  = sinip * sinop
           let mutable betdp  = sinip * cosop
           let dalf   =  ph * cosop + pinc * cosip * sinop
           let dbet   = -ph * sinop + pinc * cosip * cosop
           alfdp <- alfdp + dalf
           betdp <- betdp + dbet
           nodep <- fmod nodep twopi
           // sgp4fix for afspc written intrinsic functions
           // nodep used without a trigonometric function ahead.
           if ((nodep < 0.0) && (opsmode = 'a')) then
               nodep <- nodep + twopi
           let mutable xls = mp + argpp + cosip * nodep
           let dls    = pl + pgh - pinc * nodep * sinip
           xls <- xls + dls
           let xnoh = nodep
           nodep <- atan2 alfdp betdp
           // sgp4fix for afspc written intrinsic functions
           // nodep used without a trigonometric function ahead.
           if ((nodep < 0.0) && (opsmode = 'a')) then
               nodep <- nodep + twopi
           if (Math.Abs(xnoh - nodep) > PI) then
             if (nodep < xnoh) then
                 nodep <- nodep + twopi
             else
                 nodep <- nodep - twopi
           mp <- mp + pl
           argpp <- xls - mp - cosip * nodep

let dspace
           (irez : int)
           (d2201 : double)
           (d2211 : double)
           (d3210 : double)
           (d3222 : double)
           (d4410 : double)
           (d4422 : double)
           (d5220 : double)
           (d5232 : double)
           (d5421 : double)
           (d5433 : double)
           (dedt : double)
           (del1 : double)
           (del2 : double)
           (del3 : double)
           (didt : double)
           (dmdt : double)
           (dnodt : double)
           (domdt : double)
           (argpo : double)
           (argpdot : double)
           (t : double)
           (tc : double)
           (gsto : double)
           (xfact : double)
           (xlamo : double)
           (no : double)
           (atime : double byref)
           (em : double byref)
           (argpm : double byref)
           (inclm : double byref)
           (xli : double byref)
           (mm : double byref)
           (xni : double byref)
           (nodem : double byref)
           (dndt : double byref)
           (nm : double byref) =

    let mutable delt  = Double.NaN
    let mutable xndt  = Double.NaN
    let mutable xldot = Double.NaN
    let mutable xnddt = Double.NaN
    let mutable xomi  = Double.NaN
    let mutable x2omi = Double.NaN
    let mutable x2li  = Double.NaN
    let mutable xl    = Double.NaN

    let fasx2 = 0.13130908
    let fasx4 = 2.8843198
    let fasx6 = 0.37448087
    let g22   = 5.7686396
    let g32   = 0.95240898
    let g44   = 1.8014998
    let g52   = 1.0508330
    let g54   = 4.4108898
    let rptim = 4.37526908801129966e-3 // this equates to 7.29211514668855e-5 rad/sec
    let stepp =    720.0
    let stepn =   -720.0
    let step2 = 259200.0

    // Calculate deep space resonance effects:
    dndt   <- 0.0
    let theta = fmod (gsto + tc * rptim) twopi
    em     <- em + dedt * t

    inclm  <- inclm + didt * t
    argpm  <- argpm + domdt * t
    nodem  <- nodem + dnodt * t
    mm     <- mm + dmdt * t

    let mutable ft = 0.0;
    if (irez <> 0) then
        if ((atime = 0.0) || (t * atime <= 0.0) || (Math.Abs(t) < Math.Abs(atime)) ) then
            atime  <- 0.0
            xni    <- no
            xli    <- xlamo
        if (t > 0.0) then
            delt <- stepp
        else
            delt <- stepn

        let mutable iretn = 381
        let mutable iret  =   0
        while (iretn = 381) do
            // Dot terms calculated.
            // Near - synchronous resonance terms:
            if (irez <> 2) then
                xndt  <- del1 * sin(xli - fasx2) + del2 * sin(2.0 * (xli - fasx4)) +
                         del3 * sin(3.0 * (xli - fasx6))
                xldot <- xni + xfact;
                xnddt <- del1 * cos(xli - fasx2) +
                         2.0 * del2 * cos(2.0 * (xli - fasx4)) +
                         3.0 * del3 * cos(3.0 * (xli - fasx6))
                xnddt <- xnddt * xldot
            else
                // Near - half-day resonance terms:
                xomi  <- argpo + argpdot * atime
                x2omi <- xomi + xomi
                x2li  <- xli + xli
                xndt  <- d2201 * sin(x2omi + xli - g22) + d2211 * sin(xli - g22) +
                         d3210 * sin(xomi + xli - g32)  + d3222 * sin(-xomi + xli - g32)+
                         d4410 * sin(x2omi + x2li - g44)+ d4422 * sin(x2li - g44) +
                         d5220 * sin(xomi + xli - g52)  + d5232 * sin(-xomi + xli - g52)+
                         d5421 * sin(xomi + x2li - g54) + d5433 * sin(-xomi + x2li - g54)
                xldot <- xni + xfact
                xnddt <- d2201 * cos(x2omi + xli - g22) + d2211 * cos(xli - g22) +
                         d3210 * cos(xomi + xli - g32) + d3222 * cos(-xomi + xli - g32) +
                         d5220 * cos(xomi + xli - g52) + d5232 * cos(-xomi + xli - g52) +
                         2.0 * (d4410 * cos(x2omi + x2li - g44) +
                                d4422 * cos(x2li - g44) + d5421 * cos(xomi + x2li - g54) +
                                d5433 * cos(-xomi + x2li - g54))
                xnddt <- xnddt * xldot

            // Integrator:
            if (Math.Abs(t - atime) >= stepp) then
                iret  <- 0
                iretn <- 381
            else // exit here
                ft    <- t - atime
                iretn <- 0

            if (iretn = 381) then
                xli   <- xli + xldot * delt + xndt * step2
                xni   <- xni + xndt * delt + xnddt * step2
                atime <- atime + delt
        // while iretn = 381

        nm <- xni + xndt * ft + xnddt * ft * ft * 0.5
        xl <- xli + xldot * ft + xndt * ft * ft * 0.5
        if (irez <> 1) then
            mm   <- xl - 2.0 * nodem + 2.0 * theta
            dndt <- nm - no
        else
            mm   <- xl - nodem - argpm + theta
            dndt <- nm - no
        nm <- no + dndt

let sgp4 (whichconst : GravConstType)
         (satrec : ElSetRec)
         (tsince : double) =
    try    
        let mutable t2              = Double.NaN
        let mutable t3              = Double.NaN
        let mutable t4              = Double.NaN
        let mutable temp            = Double.NaN
        let mutable temp1           = Double.NaN
        let mutable temp2           = Double.NaN
        let mutable tempa           = Double.NaN
        let mutable tempe           = Double.NaN
        let mutable templ           = Double.NaN
        let mutable ux              = Double.NaN
        let mutable uy              = Double.NaN
        let mutable uz              = Double.NaN
        let mutable vx              = Double.NaN
        let mutable vy              = Double.NaN
        let mutable vz              = Double.NaN
        let mutable inclm           = Double.NaN
        let mutable mm              = Double.NaN
        let mutable nm              = Double.NaN
        let mutable nodem           = Double.NaN
        let mutable xinc            = Double.NaN
        let mutable xincp           = Double.NaN
        let mutable xl              = Double.NaN
        let mutable xlm             = Double.NaN
        let mutable mp              = Double.NaN
        let mutable xmdf            = Double.NaN
        let mutable xmx             = Double.NaN
        let mutable xmy             = Double.NaN
        let mutable nodedf          = Double.NaN
        let mutable xnode           = Double.NaN
        let mutable nodep           = Double.NaN
        let mutable tc              = Double.NaN
        let mutable dndt            = Double.NaN
        let mutable delmtemp        = Double.NaN


        // Set mathematical constants:
        let temp4 = 1.5e-12
        let x2o3  = 2.0 / 3.0;
        let gravConsts = getgravconst whichconst 
        //&tumin &mu &radiusearthkm &xke &j2 &j3 &j4 &j3oj2 
        let vkmpersec = gravConsts.radiusearthkm * gravConsts.xke/60.0

        // Clear sgp4 error flag:
        satrec.t     <- tsince
        satrec.error <- 0s

        // Update for secular gravity and atmospheric drag:
        xmdf    <- satrec.mo + satrec.mdot * satrec.t
        let argpdf = satrec.argpo + satrec.argpdot * satrec.t
        nodedf  <- satrec.nodeo + satrec.nodedot * satrec.t
        let mutable argpm = argpdf
        mm      <- xmdf
        t2      <- satrec.t * satrec.t
        nodem   <- nodedf + satrec.nodecf * t2
        tempa   <- 1.0 - satrec.cc1 * satrec.t
        tempe   <- satrec.bstar * satrec.cc4 * satrec.t
        templ   <- satrec.t2cof * t2

        if (satrec.isimp <> 1s) then
            let delomg = satrec.omgcof * satrec.t
            delmtemp <-  1.0 + satrec.eta * cos(xmdf)
            let delm = satrec.xmcof * (delmtemp * delmtemp * delmtemp - satrec.delmo)
            temp     <- delomg + delm
            mm       <- xmdf + temp
            argpm    <- argpdf - temp
            t3       <- t2 * satrec.t
            t4       <- t3 * satrec.t
            tempa    <- tempa - satrec.d2 * t2 - satrec.d3 * t3 -
                              satrec.d4 * t4
            tempe    <- tempe + satrec.bstar * satrec.cc5 * (sin(mm) -
                              satrec.sinmao)
            templ    <- templ + satrec.t3cof * t3 + t4 * (satrec.t4cof + satrec.t * satrec.t5cof)

        nm    <- satrec.no
        let mutable em = satrec.ecco
        inclm <- satrec.inclo
        if (satrec.method' = 'd') then
            tc <- satrec.t
            dspace
                (int(satrec.irez))
                satrec.d2201 satrec.d2211 satrec.d3210
                satrec.d3222 satrec.d4410 satrec.d4422
                satrec.d5220 satrec.d5232 satrec.d5421
                satrec.d5433 satrec.dedt  satrec.del1
                satrec.del2  satrec.del3  satrec.didt
                satrec.dmdt  satrec.dnodt satrec.domdt
                satrec.argpo satrec.argpdot satrec.t tc
                satrec.gsto satrec.xfact satrec.xlamo
                satrec.no &satrec.atime
                &em &argpm &inclm &satrec.xli &mm &satrec.xni
                &nodem &dndt &nm
        if (nm <= 0.0) then
            satrec.error <- 2s
            raise (Exception("Error 2 in sgp4"))
        else
            let am = Math.Pow((gravConsts.xke / nm),x2o3) * tempa * tempa
            nm <- gravConsts.xke / Math.Pow(am, 1.5)
            em <- em - tempe

            // fix tolerance for error recognition
            if ((em >= 1.0) || (em < -0.001)) then
                satrec.error <- 1s
                raise (Exception("Error 1 in sgp4"))
            else
                // Fix tolerance to avoid a divide by zero
                if (em < 1.0e-6) then
                    em <- 1.0e-6
                mm     <- mm + satrec.no * templ
                xlm    <- mm + argpm + nodem
                let emsq = em**2.0
                temp   <- 1.0 - emsq
                   
                nodem  <- fmod nodem twopi
                argpm  <- fmod argpm twopi
                xlm    <- fmod xlm twopi
                mm     <- fmod (xlm - argpm - nodem) twopi

                // Compute extra mean quantities:
                let sinim = sin(inclm)
                let cosim = cos(inclm)

                // Add lunar-solar periodics:
                let mutable ep = em
                xincp  <- inclm
                let mutable argpp = argpm
                nodep  <- nodem
                mp     <- mm
                let mutable sinip = sinim
                let mutable cosip = cosim
                if (satrec.method' = 'd') then
                    dpper
                        satrec.e3   satrec.ee2  satrec.peo
                        satrec.pgho satrec.pho  satrec.pinco
                        satrec.plo  satrec.se2  satrec.se3
                        satrec.sgh2 satrec.sgh3 satrec.sgh4
                        satrec.sh2  satrec.sh3  satrec.si2
                        satrec.si3  satrec.sl2  satrec.sl3
                        satrec.sl4  satrec.t    satrec.xgh2
                        satrec.xgh3 satrec.xgh4 satrec.xh2
                        satrec.xh3  satrec.xi2  satrec.xi3
                        satrec.xl2  satrec.xl3  satrec.xl4
                        satrec.zmol satrec.zmos satrec.inclo
                        'n' &ep &xincp &nodep &argpp &mp satrec.operationmode
                    if (xincp < 0.0) then
                        xincp  <- -xincp
                        nodep  <- nodep + PI
                        argpp  <- argpp - PI
                    if ((ep < 0.0 ) || ( ep > 1.0)) then
                        satrec.error <- 3s
                        raise (Exception("Error 3 in sgp4"))

                // Long period periodics:
                if (satrec.method' = 'd') then
                    sinip <-  sin(xincp)
                    cosip <-  cos(xincp)
                    satrec.aycof <- -0.5*gravConsts.j3oj2*sinip
                    if (Math.Abs(cosip+1.0) > 1.5e-12) then
                        satrec.xlcof <- -0.25 * gravConsts.j3oj2 * sinip * (3.0 + 5.0 * cosip) / (1.0 + cosip)
                    else
                        satrec.xlcof <- -0.25 * gravConsts.j3oj2 * sinip * (3.0 + 5.0 * cosip) / temp4

                let axnl = ep * cos(argpp)
                temp <- 1.0 / (am * (1.0 - ep * ep))
                let aynl = ep* sin(argpp) + temp * satrec.aycof
                xl   <- mp + argpp + nodep + temp * satrec.xlcof * axnl

                // Solve kepler's equation:
                let mutable u      = Double.NaN
                let mutable coseo1 = Double.NaN
                let mutable eo1    = Double.NaN
                let mutable tem5   = Double.NaN
                let mutable ktr    = Int32.MinValue
                let mutable sineo1 = Double.NaN

                u    <- fmod (xl - nodep) twopi
                eo1  <- u
                tem5 <- 9999.9
                ktr  <- 1
                while (( Math.Abs(tem5) >= 1.0e-12) && (ktr <= 10) ) do
                    sineo1 <- sin(eo1)
                    coseo1 <- cos(eo1)
                    tem5   <- 1.0 - coseo1 * axnl - sineo1 * aynl
                    tem5   <- (u - aynl * coseo1 + axnl * sineo1 - eo1) / tem5
                    if (Math.Abs(tem5) >= 0.95) then
                        tem5 <- if tem5 > 0.0 then 0.95 else -0.95
                    eo1 <- eo1 + tem5
                    ktr <- ktr + 1

                // Short period preliminary quantities:
                let ecose = axnl*coseo1 + aynl*sineo1
                let esine = axnl*sineo1 - aynl*coseo1
                let el2 = axnl*axnl + aynl*aynl
                let pl = am*(1.0-el2)
                if (pl < 0.0) then
                    satrec.error <- 4s
                    raise (Exception("Error 4 in sgp4"))
                else
                    let rl = am * (1.0 - ecose)
                    let rdotl = sqrt(am) * esine/rl
                    let rvdotl = sqrt(pl) / rl
                    let betal = sqrt(1.0 - el2)
                    temp   <- esine / (1.0 + betal)
                    let sinu = am / rl * (sineo1 - aynl - axnl * temp)
                    let cosu = am / rl * (coseo1 - axnl + aynl * temp)
                    let mutable su = atan2 sinu cosu
                    let sin2u = (cosu + cosu) * sinu
                    let cos2u = 1.0 - 2.0 * sinu * sinu
                    temp   <- 1.0 / pl
                    temp1  <- 0.5 * gravConsts.j2 * temp
                    temp2  <- temp1 * temp

                    // Update for short period periodics:
                    if (satrec.method' = 'd') then
                        let cosisq = cosip * cosip
                        satrec.con41  <- 3.0*cosisq - 1.0
                        satrec.x1mth2 <- 1.0 - cosisq
                        satrec.x7thm1 <- 7.0*cosisq - 1.0

                    let mrt = rl * (1.0 - 1.5 * temp2 * betal * satrec.con41) +
                               0.5 * temp1 * satrec.x1mth2 * cos2u
                    su    <- su - 0.25 * temp2 * satrec.x7thm1 * sin2u
                    xnode <- nodep + 1.5 * temp2 * cosip * sin2u
                    xinc  <- xincp + 1.5 * temp2 * cosip * sinip * cos2u
                    let mvt = rdotl - nm * temp1 * satrec.x1mth2 * sin2u / gravConsts.xke
                    let rvdot = rvdotl + nm * temp1 * (satrec.x1mth2 * cos2u +
                                 1.5 * satrec.con41) / gravConsts.xke

                    // Orientation vectors:
                    let sinsu = sin(su)
                    let cossu = cos(su)
                    let snod = sin(xnode)
                    let cnod = cos(xnode)
                    let sini = sin(xinc)
                    let cosi = cos(xinc)
                    xmx   <- -snod * cosi
                    xmy   <- cnod * cosi
                    ux    <- xmx * sinsu + cnod * cossu
                    uy    <- xmy * sinsu + snod * cossu
                    uz    <- sini * sinsu
                    vx    <- xmx * cossu - cnod * sinsu
                    vy    <- xmy * cossu - snod * sinsu
                    vz    <- sini * cossu

                    if (mrt < 1.0) then
                        satrec.error <- 6s
                        raise (Exception("Error 6 in sgp4"))

                    // Position and velocity (in km and km/sec):
                    // Success:
                    (
                        true,

                        // Position:
                        [| ux; uy; uz |]
                        |> Array.map (fun u -> (mrt * u) * gravConsts.radiusearthkm)
                        |> Some,

                        // Velocity
                        [| ux, vx; uy, vy; uz, vz |]
                        |> Array.map (fun (u,v) -> (mvt * u + rvdot * v) * vkmpersec)
                        |> Some
                    ) 
    with
    | _ -> (false, None, None)

let dscom (epoch : double)
          (ep : double)
          (argpp : double)
          (tc : double)
          (inclp: double)
          (nodep : double)
          (np: double)
          (snodm : double byref)
          (cnodm : double byref)
          (sinim : double byref)
          (cosim : double byref)
          (sinomm: double byref)
          (cosomm: double byref)
          (day : double byref)
          (e3 : double byref)
          (ee2 : double byref)
          (em: double byref)
          (emsq : double byref)
          (gam : double byref)
          (peo : double byref)
          (pgho : double byref)
          (pho: double byref)
          (pinco : double byref)
          (plo : double byref)
          (rtemsq : double byref)
          (se2 : double byref)
          (se3: double byref)
          (sgh2 : double byref)
          (sgh3 : double byref)
          (sgh4 : double byref)
          (sh2 : double byref)
          (sh3: double byref)
          (si2 : double byref)
          (si3 : double byref)
          (sl2 : double byref)
          (sl3 : double byref)
          (sl4: double byref)
          (s1 : double byref)
          (s2 : double byref)
          (s3 : double byref)
          (s4 : double byref)
          (s5: double byref)
          (s6 : double byref)
          (s7 : double byref)
          (ss1 : double byref)
          (ss2 : double byref)
          (ss3: double byref)
          (ss4 : double byref)
          (ss5 : double byref)
          (ss6 : double byref)
          (ss7 : double byref)
          (sz1: double byref)
          (sz2 : double byref)
          (sz3 : double byref)
          (sz11 : double byref)
          (sz12 : double byref)
          (sz13: double byref)
          (sz21 : double byref)
          (sz22 : double byref)
          (sz23 : double byref)
          (sz31 : double byref)
          (sz32: double byref)
          (sz33 : double byref)
          (xgh2 : double byref)
          (xgh3 : double byref)
          (xgh4 : double byref)
          (xh2: double byref)
          (xh3 : double byref)
          (xi2 : double byref)
          (xi3 : double byref)
          (xl2 : double byref)
          (xl3: double byref)
          (xl4 : double byref)
          (nm : double byref)
          (z1 : double byref)
          (z2 : double byref)
          (z3: double byref)
          (z11 : double byref)
          (z12 : double byref)
          (z13 : double byref)
          (z21 : double byref)
          (z22: double byref)
          (z23 : double byref)
          (z31 : double byref)
          (z32 : double byref)
          (z33 : double byref)
          (zmol: double byref)
          (zmos: double byref) =

    // Constants:
    let zes     =  0.01675
    let zel     =  0.05490
    let c1ss    =  2.9864797e-6
    let c1l     =  4.7968065e-7
    let zsinis  =  0.39785416
    let zcosis  =  0.91744867
    let zcosgs  =  0.1945905
    let zsings  = -0.98088458

    let mutable a1     = Double.NaN
    let mutable a2     = Double.NaN
    let mutable a3     = Double.NaN
    let mutable a4     = Double.NaN
    let mutable a5     = Double.NaN
    let mutable a6     = Double.NaN
    let mutable a7     = Double.NaN
    let mutable a8     = Double.NaN
    let mutable a9     = Double.NaN
    let mutable a10    = Double.NaN
    let mutable betasq = Double.NaN
    let mutable cc     = Double.NaN
    let mutable ctem   = Double.NaN
    let mutable stem   = Double.NaN
    let mutable x1     = Double.NaN
    let mutable x2     = Double.NaN
    let mutable x3     = Double.NaN
    let mutable x4     = Double.NaN
    let mutable x5     = Double.NaN
    let mutable x6     = Double.NaN
    let mutable x7     = Double.NaN
    let mutable x8     = Double.NaN
    let mutable xnodce = Double.NaN
    let mutable xnoi   = Double.NaN
    let mutable zcosg  = Double.NaN
    let mutable zcosgl = Double.NaN
    let mutable zcosh  = Double.NaN
    let mutable zcoshl = Double.NaN
    let mutable zcosi  = Double.NaN
    let mutable zcosil = Double.NaN
    let mutable zsing  = Double.NaN
    let mutable zsingl = Double.NaN
    let mutable zsinh  = Double.NaN
    let mutable zsinhl = Double.NaN
    let mutable zsini  = Double.NaN
    let mutable zsinil = Double.NaN
    let mutable zx     = Double.NaN
    let mutable zy     = Double.NaN

    nm     <- np
    em     <- ep
    snodm  <- sin(nodep)
    cnodm  <- cos(nodep)
    sinomm <- sin(argpp)
    cosomm <- cos(argpp)
    sinim  <- sin(inclp)
    cosim  <- cos(inclp)
    emsq   <- em * em
    betasq <- 1.0 - emsq
    rtemsq <- sqrt(betasq)

    // Initialize lunar solar terms:
    peo    <- 0.0
    pinco  <- 0.0
    plo    <- 0.0
    pgho   <- 0.0
    pho    <- 0.0
    day    <- epoch + 18261.5 + tc / minperday
    xnodce <- fmod (4.5236020 - 9.2422029e-4 * day) twopi
    stem   <- sin(xnodce)
    ctem   <- cos(xnodce)
    zcosil <- 0.91375164 - 0.03568096 * ctem
    zsinil <- sqrt(1.0 - zcosil * zcosil)
    zsinhl <- 0.089683511 * stem / zsinil
    zcoshl <- sqrt(1.0 - zsinhl * zsinhl)
    gam    <- 5.8351514 + 0.0019443680 * day
    zx     <- 0.39785416 * stem / zsinil
    zy     <- zcoshl * ctem + 0.91744867 * zsinhl * stem
    zx     <- atan2 zx zy
    zx     <- gam + zx - xnodce
    zcosgl <- cos(zx)
    zsingl <- sin(zx)

    // Do solar terms:
    let mutable zcosg = zcosgs
    let mutable zsing = zsings
    let mutable zcosi = zcosis
    let mutable zsini = zsinis
    let mutable zcosh = cnodm
    let mutable zsinh = snodm
    let mutable cc    = c1ss
    let xnoi  = 1.0 / nm

    for lsflg in 1..2 do
        a1  <-   zcosg * zcosh + zsing * zcosi * zsinh
        a3  <-  -zsing * zcosh + zcosg * zcosi * zsinh
        a7  <-  -zcosg * zsinh + zsing * zcosi * zcosh
        a8  <-   zsing * zsini
        a9  <-   zsing * zsinh + zcosg * zcosi * zcosh
        a10 <-   zcosg * zsini
        a2  <-   cosim * a7 + sinim * a8
        a4  <-   cosim * a9 + sinim * a10
        a5  <-  -sinim * a7 + cosim * a8
        a6  <-  -sinim * a9 + cosim * a10
        
        x1  <-  a1 * cosomm + a2 * sinomm
        x2  <-  a3 * cosomm + a4 * sinomm
        x3  <- -a1 * sinomm + a2 * cosomm
        x4  <- -a3 * sinomm + a4 * cosomm
        x5  <-  a5 * sinomm
        x6  <-  a6 * sinomm
        x7  <-  a5 * cosomm
        x8  <-  a6 * cosomm
        
        z31 <- 12.0 * x1 * x1 - 3.0 * x3 * x3
        z32 <- 24.0 * x1 * x2 - 6.0 * x3 * x4
        z33 <- 12.0 * x2 * x2 - 3.0 * x4 * x4
        z1  <-  3.0 *  (a1 * a1 + a2 * a2) + z31 * emsq
        z2  <-  6.0 *  (a1 * a3 + a2 * a4) + z32 * emsq
        z3  <-  3.0 *  (a3 * a3 + a4 * a4) + z33 * emsq
        z11 <- -6.0 * a1 * a5 + emsq *  (-24.0 * x1 * x7-6.0 * x3 * x5)
        z12 <- -6.0 *  (a1 * a6 + a3 * a5) + emsq *
               (-24.0 * (x2 * x7 + x1 * x8) - 6.0 * (x3 * x6 + x4 * x5))
        z13 <- -6.0 * a3 * a6 + emsq * (-24.0 * x2 * x8 - 6.0 * x4 * x6)
        z21 <-  6.0 * a2 * a5 + emsq * (24.0 * x1 * x5 - 6.0 * x3 * x7)
        z22 <-  6.0 *  (a4 * a5 + a2 * a6) + emsq *
               (24.0 * (x2 * x5 + x1 * x6) - 6.0 * (x4 * x7 + x3 * x8))
        z23 <-  6.0 * a4 * a6 + emsq * (24.0 * x2 * x6 - 6.0 * x4 * x8)
        z1  <- z1 + z1 + betasq * z31
        z2  <- z2 + z2 + betasq * z32
        z3  <- z3 + z3 + betasq * z33
        s3  <- cc * xnoi
        s2  <- -0.5 * s3 / rtemsq
        s4  <- s3 * rtemsq
        s1  <- -15.0 * em * s4
        s5  <- x1 * x3 + x2 * x4
        s6  <- x2 * x3 + x1 * x4
        s7  <- x2 * x4 - x1 * x3

        // Do lunar terms:
        if (lsflg = 1) then
            ss1   <- s1
            ss2   <- s2
            ss3   <- s3
            ss4   <- s4
            ss5   <- s5
            ss6   <- s6
            ss7   <- s7
            sz1   <- z1
            sz2   <- z2
            sz3   <- z3
            sz11  <- z11
            sz12  <- z12
            sz13  <- z13
            sz21  <- z21
            sz22  <- z22
            sz23  <- z23
            sz31  <- z31
            sz32  <- z32
            sz33  <- z33
            zcosg <- zcosgl
            zsing <- zsingl
            zcosi <- zcosil
            zsini <- zsinil
            zcosh <- zcoshl * cnodm + zsinhl * snodm
            zsinh <- snodm * zcoshl - cnodm * zsinhl
            cc    <- c1l
        
    zmol <- fmod (4.7199672 + 0.22997150  * day - gam) twopi
    zmos <- fmod (6.2565837 + 0.017201977 * day) twopi

    // Do solar terms:
    se2  <-   2.0 * ss1 * ss6
    se3  <-   2.0 * ss1 * ss7
    si2  <-   2.0 * ss2 * sz12
    si3  <-   2.0 * ss2 * (sz13 - sz11)
    sl2  <-  -2.0 * ss3 * sz2
    sl3  <-  -2.0 * ss3 * (sz3 - sz1)
    sl4  <-  -2.0 * ss3 * (-21.0 - 9.0 * emsq) * zes
    sgh2 <-   2.0 * ss4 * sz32
    sgh3 <-   2.0 * ss4 * (sz33 - sz31)
    sgh4 <- -18.0 * ss4 * zes
    sh2  <-  -2.0 * ss2 * sz22
    sh3  <-  -2.0 * ss2 * (sz23 - sz21)

     // Do solar terms
    ee2  <-   2.0 * s1 * s6
    e3   <-   2.0 * s1 * s7
    xi2  <-   2.0 * s2 * z12
    xi3  <-   2.0 * s2 * (z13 - z11)
    xl2  <-  -2.0 * s3 * z2
    xl3  <-  -2.0 * s3 * (z3 - z1)
    xl4  <-  -2.0 * s3 * (-21.0 - 9.0 * emsq) * zel
    xgh2 <-   2.0 * s4 * z32
    xgh3 <-   2.0 * s4 * (z33 - z31)
    xgh4 <- -18.0 * s4 * zel
    xh2  <-  -2.0 * s2 * z22
    xh3  <-  -2.0 * s2 * (z23 - z21)

let dsinit (whichconst : GravConstType)
           (cosim : double)
           (emsq : double)
           (argpo : double)
           (s1 : double)
           (s2 : double)
           (s3 : double)
           (s4 : double)
           (s5 : double)
           (sinim : double)
           (ss1 : double)
           (ss2 : double)
           (ss3 : double)
           (ss4 : double)
           (ss5 : double)
           (sz1 : double)
           (sz3 : double)
           (sz11 : double)
           (sz13 : double)
           (sz21 : double)
           (sz23 : double)
           (sz31 : double)
           (sz33 : double)
           (t : double)
           (tc : double)
           (gsto : double)
           (mo : double)
           (mdot : double)
           (no : double)
           (nodeo : double)
           (nodedot : double)
           (xpidot : double)
           (z1 : double)
           (z3 : double)
           (z11 : double)
           (z13 : double)
           (z21 : double)
           (z23 : double)
           (z31 : double)
           (z33 : double)
           (ecco : double)
           (eccsq : double)
           (em : double byref)
           (argpm : double byref)
           (inclm : double byref)
           (mm : double byref)
           (nm : double byref)
           (nodem : double byref)
           (irez : int16 byref)
           (atime : double byref)
           (d2201 : double byref)
           (d2211 : double byref)
           (d3210 : double byref)
           (d3222 : double byref)
           (d4410 : double byref)
           (d4422 : double byref)
           (d5220 : double byref)
           (d5232 : double byref)
           (d5421 : double byref)
           (d5433 : double byref)
           (dedt : double byref)
           (didt : double byref)
           (dmdt : double byref)
           (dndt : double byref)
           (dnodt : double byref)
           (domdt : double byref)
           (del1 : double byref)
           (del2 : double byref)
           (del3 : double byref)
           (xfact : double byref)
           (xlamo : double byref)
           (xli : double byref)
           (xni : double byref) =

     // Local variables:
    let mutable aonv = 0.0

    let mutable f220 = Double.NaN
    let mutable f221 = Double.NaN
    let mutable f311 = Double.NaN
    let mutable f321 = Double.NaN
    let mutable f322 = Double.NaN
    let mutable f330 = Double.NaN
    let mutable f441 = Double.NaN
    let mutable f442 = Double.NaN
    let mutable f522 = Double.NaN
    let mutable f523 = Double.NaN
    let mutable f542 = Double.NaN
    let mutable f543 = Double.NaN

    let mutable g200 = Double.NaN
    let mutable g211 = Double.NaN
    let mutable g300 = Double.NaN
    let mutable g310 = Double.NaN
    let mutable g310 = Double.NaN
    let mutable g322 = Double.NaN
    let mutable g410 = Double.NaN
    let mutable g422 = Double.NaN
    let mutable g520 = Double.NaN
    let mutable g521 = Double.NaN
    let mutable g532 = Double.NaN
    let mutable g533 = Double.NaN

    let mutable emsq' = emsq // In C++ code is passed by value and assigned to in body

    let q22    = 1.7891679e-6
    let q31    = 2.1460748e-6
    let q33    = 2.2123015e-7
    let root22 = 1.7891679e-6
    let root44 = 7.3636953e-9
    let root54 = 2.1765803e-9
    let rptim  = 4.37526908801129966e-3 // this equates to 7.29211514668855e-5 rad/sec
    let root32 = 3.7393792e-7
    let root52 = 1.1428639e-7
    let x2o3   = 2.0 / 3.0
    let znl    = 1.5835218e-4
    let zns    = 1.19459e-5

    // sgp4fix identify constants and allow alternate values
    let gravConsts = getgravconst whichconst

    // Deep space initialization:
    irez <- 0s
    if ((nm < 0.0052359877) && (nm > 0.0034906585)) then
        irez <- 1s
    if ((nm >= 8.26e-3) && (nm <= 9.24e-3) && (em >= 0.5)) then
        irez <- 2s

    // Do solar terms:
    let ses  =  ss1 * zns * ss5
    let sis  =  ss2 * zns * (sz11 + sz13)
    let sls  = -zns * ss3 * (sz1 + sz3 - 14.0 - 6.0 * emsq')
    let sghs =  ss4 * zns * (sz31 + sz33 - 6.0)
    let mutable shs  = -zns * ss2 * (sz21 + sz23)
    // sgp4fix for 180 deg incl
    if ((inclm < 5.2359877e-2) || (inclm > PI - 5.2359877e-2)) then
        shs <- 0.0
    if (sinim <> 0.0) then
        shs <- shs / sinim
    let sgs = sghs - cosim * shs

    // Do lunar terms:
    dedt <- ses + s1 * znl * s5
    didt <- sis + s2 * znl * (z11 + z13)
    dmdt <- sls - znl * s3 * (z1 + z3 - 14.0 - 6.0 * emsq')
    let sghl = s4 * znl * (z31 + z33 - 6.0)
    let mutable shll = -znl * s2 * (z21 + z23)
    // sgp4fix for 180 deg incl
    if ((inclm < 5.2359877e-2) || (inclm > PI - 5.2359877e-2)) then
        shll <- 0.0
    domdt <- sgs + sghl
    dnodt <- shs
    if (sinim <> 0.0) then
        domdt <- domdt - cosim / sinim * shll
        dnodt <- dnodt + shll / sinim

    // Calculate deep space resonance effects:
    dndt   <- 0.0
    let theta  = fmod (gsto + tc * rptim) twopi
    em     <- em + dedt * t
    inclm  <- inclm + didt * t
    argpm  <- argpm + domdt * t
    nodem  <- nodem + dnodt * t
    mm     <- mm + dmdt * t

    // Initialize the resonance terms:
    if (irez <> 0s) then
        aonv <- Math.Pow(nm / gravConsts.xke, x2o3)

        // TODO I think that some of these are uninitialized in the C++ code - a bug?
        let mutable cosisq = Double.NaN
        let mutable emo = Double.NaN
        let mutable emsqo = Double.NaN
        let mutable eoc = Double.NaN
        let mutable g201 = Double.NaN
        // Geopotential resonance for 12 hour orbits:
        if (irez = 2s) then
            cosisq <- cosim * cosim
            emo    <- em
            em     <- ecco
            emsqo  <- emsq'
            emsq'  <- eccsq
            eoc    <- em * emsq'
            g201   <- -0.306 - (em - 0.64) * 0.440

            if (em <= 0.65) then
                g211 <-    3.616  -  13.2470 * em +  16.2900 * emsq'
                g310 <-  -19.302  + 117.3900 * em - 228.4190 * emsq'+  156.5910 * eoc
                g322 <-  -18.9068 + 109.7927 * em - 214.6334 * emsq'+  146.5816 * eoc
                g410 <-  -41.122  + 242.6940 * em - 471.0940 * emsq'+  313.9530 * eoc
                g422 <- -146.407  + 841.8800 * em - 1629.014 * emsq'+ 1083.4350 * eoc
                g520 <- -532.114  + 3017.977 * em - 5740.032 * emsq'+ 3708.2760 * eoc
            else
                g211 <-   -72.099 +   331.819 * em -   508.738 * emsq'+   266.724 * eoc
                g310 <-  -346.844 +  1582.851 * em -  2415.925 * emsq'+  1246.113 * eoc
                g322 <-  -342.585 +  1554.908 * em -  2366.899 * emsq'+  1215.972 * eoc
                g410 <- -1052.797 +  4758.686 * em -  7193.992 * emsq'+  3651.957 * eoc
                g422 <- -3581.690 + 16178.110 * em - 24462.770 * emsq'+ 12422.520 * eoc
                if (em > 0.715) then
                    g520 <- -5149.66 + 29936.92 * em - 54087.36 * emsq'+ 31324.56 * eoc
                else
                    g520 <- 1464.74 -  4664.75 * em +  3763.64 * emsq'
            if (em < 0.7) then
                g533 <- -919.22770 + 4988.6100 * em - 9064.7700 * emsq'+ 5542.21  * eoc
                g521 <- -822.71072 + 4568.6173 * em - 8491.4146 * emsq'+ 5337.524 * eoc
                g532 <- -853.66600 + 4690.2500 * em - 8624.7700 * emsq'+ 5341.4  * eoc
            else     
                g533 <- -37995.780 + 161616.52 * em - 229838.20 * emsq'+ 109377.94 * eoc
                g521 <- -51752.104 + 218913.95 * em - 309468.16 * emsq'+ 146349.42 * eoc
                g532 <- -40023.880 + 170470.89 * em - 242699.48 * emsq'+ 115605.82 * eoc

            let sini2 =  sinim * sinim
            f220 <-  0.75 * (1.0 + 2.0 * cosim+cosisq)
            f221 <-  1.5 * sini2
            f321 <-  1.875 * sinim  *  (1.0 - 2.0 * cosim - 3.0 * cosisq)
            f322 <- -1.875 * sinim  *  (1.0 + 2.0 * cosim - 3.0 * cosisq)
            f441 <- 35.0 * sini2 * f220
            f442 <- 39.3750 * sini2 * sini2
            f522 <-  9.84375 * sinim * (sini2 * (1.0 - 2.0 * cosim- 5.0 * cosisq) +
                      0.33333333 * (-2.0 + 4.0 * cosim + 6.0 * cosisq) )
            f523 <- sinim * (4.92187512 * sini2 * (-2.0 - 4.0 * cosim + 10.0 * cosisq) +
                      6.56250012 * (1.0+2.0 * cosim - 3.0 * cosisq))
            f542 <- 29.53125 * sinim * (2.0 - 8.0 * cosim+cosisq *
                     (-12.0 + 8.0 * cosim + 10.0 * cosisq))
            f543 <- 29.53125 * sinim * (-2.0 - 8.0 * cosim+cosisq *
                         (12.0 + 8.0 * cosim - 10.0 * cosisq))
            let xno2  =  nm * nm
            let ainv2 =  aonv * aonv
            let mutable temp1 =  3.0 * xno2 * ainv2
            let mutable temp  =  temp1 * root22
            d2201 <-  temp * f220 * g201
            d2211 <-  temp * f221 * g211
            temp1 <-  temp1 * aonv
            temp  <-  temp1 * root32
            d3210 <-  temp * f321 * g310
            d3222 <-  temp * f322 * g322
            temp1 <-  temp1 * aonv
            temp  <-  2.0 * temp1 * root44
            d4410 <-  temp * f441 * g410
            d4422 <-  temp * f442 * g422
            temp1 <-  temp1 * aonv
            temp  <-  temp1 * root52
            d5220 <-  temp * f522 * g520
            d5232 <-  temp * f523 * g532
            temp  <-  2.0 * temp1 * root54
            d5421 <-  temp * f542 * g521
            d5433 <-  temp * f543 * g533
            xlamo <-  fmod (mo + nodeo + nodeo-theta - theta) twopi
            xfact <-  mdot + dmdt + 2.0 * (nodedot + dnodt - rptim) - no
            em    <- emo
            emsq' <- emsqo

        // Synchronous resonance terms
        if (irez = 1s) then
            g200  <- 1.0 + emsq * (-2.5 + 0.8125 * emsq)
            g310  <- 1.0 + 2.0 * emsq
            g300  <- 1.0 + emsq * (-6.0 + 6.60937 * emsq)
            f220  <- 0.75 * (1.0 + cosim) * (1.0 + cosim)
            f311  <- 0.9375 * sinim * sinim * (1.0 + 3.0 * cosim) - 0.75 * (1.0 + cosim)
            f330  <- 1.0 + cosim
            f330  <- 1.875 * f330 * f330 * f330
            del1  <- 3.0 * nm * nm * aonv * aonv
            del2  <- 2.0 * del1 * f220 * g200 * q22
            del3  <- 3.0 * del1 * f330 * g300 * q33 * aonv
            del1  <- del1 * f311 * g310 * q31 * aonv
            xlamo <- fmod (mo + nodeo + argpo - theta) twopi
            xfact <- mdot + xpidot - rptim + dmdt + domdt + dnodt - no

        // For sgp4  initialize the integrator:
        xli   <- xlamo
        xni   <- no
        atime <- 0.0
        nm    <- no + dndt

let sgp4init (whichconst : GravConstType)
             (opsmode : char)
             (satn : int16)
             (epoch : double)
             (xbstar : double)
             (xecco : double)
             (xargpo : double)
             (xinclo : double)
             (xmo : double)
             (xno : double)
             (xnodeo : double)
             (satrec : ElSetRec) =
    let mutable cnodm         = Double.NaN    
    let mutable snodm         = Double.NaN    
    let mutable cosim         = Double.NaN    
    let mutable sinim         = Double.NaN    
    let mutable cosomm        = Double.NaN    
    let mutable sinomm        = Double.NaN    
    let mutable cc1sq         = Double.NaN    
    let mutable cc2           = Double.NaN    
    let mutable cc3           = Double.NaN    
    let mutable coef          = Double.NaN    
    let mutable coef1         = Double.NaN    
    let mutable cosio4        = Double.NaN    
    let mutable day           = Double.NaN    
    let mutable dndt          = Double.NaN    
    let mutable em            = Double.NaN    
    let mutable emsq          = Double.NaN    
    let mutable eeta          = Double.NaN    
    let mutable etasq         = Double.NaN    
    let mutable gam           = Double.NaN    
    let mutable argpm         = Double.NaN    
    let mutable nodem         = Double.NaN    
    let mutable inclm         = Double.NaN    
    let mutable mm            = Double.NaN    
    let mutable nm            = Double.NaN    
    let mutable perige        = Double.NaN    
    let mutable pinvsq        = Double.NaN    
    let mutable psisq         = Double.NaN    
    let mutable qzms24        = Double.NaN    
    let mutable rtemsq        = Double.NaN    
    let mutable s1            = Double.NaN    
    let mutable s2            = Double.NaN    
    let mutable s3            = Double.NaN    
    let mutable s4            = Double.NaN    
    let mutable s5            = Double.NaN    
    let mutable s6            = Double.NaN    
    let mutable s7            = Double.NaN    
    let mutable sfour         = Double.NaN    
    let mutable ss1           = Double.NaN    
    let mutable ss2           = Double.NaN    
    let mutable ss3           = Double.NaN    
    let mutable ss4           = Double.NaN    
    let mutable ss5           = Double.NaN    
    let mutable ss6           = Double.NaN    
    let mutable ss7           = Double.NaN    
    let mutable sz1           = Double.NaN    
    let mutable sz2           = Double.NaN    
    let mutable sz3           = Double.NaN    
    let mutable sz11          = Double.NaN    
    let mutable sz12          = Double.NaN    
    let mutable sz13          = Double.NaN    
    let mutable sz21          = Double.NaN    
    let mutable sz22          = Double.NaN    
    let mutable sz23          = Double.NaN    
    let mutable sz31          = Double.NaN    
    let mutable sz32          = Double.NaN    
    let mutable sz33          = Double.NaN    
    let mutable tc            = Double.NaN    
    let mutable temp          = Double.NaN    
    let mutable temp1         = Double.NaN    
    let mutable temp2         = Double.NaN    
    let mutable temp3         = Double.NaN    
    let mutable tsi           = Double.NaN    
    let mutable xpidot        = Double.NaN    
    let mutable xhdot1        = Double.NaN    
    let mutable z1            = Double.NaN    
    let mutable z2            = Double.NaN    
    let mutable z3            = Double.NaN    
    let mutable z11           = Double.NaN    
    let mutable z12           = Double.NaN    
    let mutable z13           = Double.NaN    
    let mutable z21           = Double.NaN    
    let mutable z22           = Double.NaN    
    let mutable z23           = Double.NaN    
    let mutable z31           = Double.NaN    
    let mutable z32           = Double.NaN    
    let mutable z33           = Double.NaN    
    let mutable qzms2t        = Double.NaN
    let mutable ss            = Double.NaN
    let mutable x2o3          = Double.NaN
    let mutable delmotemp     = Double.NaN
    let mutable qzms2ttemp    = Double.NaN
    let mutable qzms24temp    = Double.NaN

    let temp4    =   1.5e-12

    // Set all near earth variables to zero:
    satrec.isimp   <- 0s;   satrec.method' <- 'n'; satrec.aycof    <- 0.0;
    satrec.con41   <- 0.0; satrec.cc1    <- 0.0; satrec.cc4      <- 0.0;
    satrec.cc5     <- 0.0; satrec.d2     <- 0.0; satrec.d3       <- 0.0;
    satrec.d4      <- 0.0; satrec.delmo  <- 0.0; satrec.eta      <- 0.0;
    satrec.argpdot <- 0.0; satrec.omgcof <- 0.0; satrec.sinmao   <- 0.0;
    satrec.t       <- 0.0; satrec.t2cof  <- 0.0; satrec.t3cof    <- 0.0;
    satrec.t4cof   <- 0.0; satrec.t5cof  <- 0.0; satrec.x1mth2   <- 0.0;
    satrec.x7thm1  <- 0.0; satrec.mdot   <- 0.0; satrec.nodedot  <- 0.0;
    satrec.xlcof   <- 0.0; satrec.xmcof  <- 0.0; satrec.nodecf   <- 0.0;

    // Set all deep space variables to zero:
    satrec.irez  <- 0s;   satrec.d2201 <- 0.0; satrec.d2211 <- 0.0;
    satrec.d3210 <- 0.0; satrec.d3222 <- 0.0; satrec.d4410 <- 0.0;
    satrec.d4422 <- 0.0; satrec.d5220 <- 0.0; satrec.d5232 <- 0.0;
    satrec.d5421 <- 0.0; satrec.d5433 <- 0.0; satrec.dedt  <- 0.0;
    satrec.del1  <- 0.0; satrec.del2  <- 0.0; satrec.del3  <- 0.0;
    satrec.didt  <- 0.0; satrec.dmdt  <- 0.0; satrec.dnodt <- 0.0;
    satrec.domdt <- 0.0; satrec.e3    <- 0.0; satrec.ee2   <- 0.0;
    satrec.peo   <- 0.0; satrec.pgho  <- 0.0; satrec.pho   <- 0.0;
    satrec.pinco <- 0.0; satrec.plo   <- 0.0; satrec.se2   <- 0.0;
    satrec.se3   <- 0.0; satrec.sgh2  <- 0.0; satrec.sgh3  <- 0.0;
    satrec.sgh4  <- 0.0; satrec.sh2   <- 0.0; satrec.sh3   <- 0.0;
    satrec.si2   <- 0.0; satrec.si3   <- 0.0; satrec.sl2   <- 0.0;
    satrec.sl3   <- 0.0; satrec.sl4   <- 0.0; satrec.gsto  <- 0.0;
    satrec.xfact <- 0.0; satrec.xgh2  <- 0.0; satrec.xgh3  <- 0.0;
    satrec.xgh4  <- 0.0; satrec.xh2   <- 0.0; satrec.xh3   <- 0.0;
    satrec.xi2   <- 0.0; satrec.xi3   <- 0.0; satrec.xl2   <- 0.0;
    satrec.xl3   <- 0.0; satrec.xl4   <- 0.0; satrec.xlamo <- 0.0;
    satrec.zmol  <- 0.0; satrec.zmos  <- 0.0; satrec.atime <- 0.0;
    satrec.xli   <- 0.0; satrec.xni   <- 0.0;

    satrec.bstar   <- xbstar
    satrec.ecco    <- xecco
    satrec.argpo   <- xargpo
    satrec.inclo   <- xinclo
    satrec.mo      <-  xmo
    satrec.no      <-  xno
    satrec.nodeo   <- xnodeo

    satrec.operationmode <- opsmode

    // Earth constants:
    let gravConsts = getgravconst whichconst
    ss <- 78.0 / gravConsts.radiusearthkm + 1.0
    // sgp4fix use multiply for speed instead of pow
    qzms2ttemp <- (120.0 - 78.0) / gravConsts.radiusearthkm
    qzms2t <- qzms2ttemp * qzms2ttemp * qzms2ttemp * qzms2ttemp
    x2o3 <- 2.0 / 3.0

    satrec.init <- 'y'
    satrec.t <- 0.0

    let initlResult = initl (int(satn)) whichconst satrec.ecco epoch satrec.inclo satrec.no satrec.operationmode

    satrec.no <- initlResult.no
    satrec.method' <- initlResult.method'
    satrec.gsto <- initlResult.gsto
    satrec.con41 <- initlResult.con41
    satrec.error <- 0s

    if ((initlResult.omeosq >= 0.0 ) || ( satrec.no >= 0.0)) then
        satrec.isimp <- 0s
        if (initlResult.rp < (220.0 / gravConsts.radiusearthkm + 1.0)) then
            satrec.isimp <- 1s
        sfour  <- ss
        qzms24 <- qzms2t
        perige <- (initlResult.rp - 1.0) * gravConsts.radiusearthkm

        // For perigees below 156 km, s and qoms2t are altered:
        if (perige < 156.0) then
            sfour <- perige - 78.0
            if (perige < 98.0) then
                sfour <- 20.0
            // sgp4fix use multiply for speed instead of pow
            qzms24temp <-  (120.0 - sfour) / gravConsts.radiusearthkm
            qzms24 <- qzms24temp * qzms24temp * qzms24temp * qzms24temp
            sfour  <- sfour / gravConsts.radiusearthkm + 1.0
        pinvsq <- 1.0 / initlResult.posq

        tsi  <- 1.0 / (initlResult.ao - sfour)
        satrec.eta  <- initlResult.ao * satrec.ecco * tsi
        etasq <- satrec.eta * satrec.eta
        eeta  <- satrec.ecco * satrec.eta
        psisq <- Math.Abs(1.0 - etasq)
        coef  <- qzms24 * Math.Pow(tsi, 4.0)
        coef1 <- coef / Math.Pow(psisq, 3.5)
        cc2   <- coef1 * satrec.no * (initlResult.ao * (1.0 + 1.5 * etasq + eeta *
                                                                      (4.0 + etasq)) + 0.375 * gravConsts.j2 * tsi / psisq * satrec.con41 *
                                                                      (8.0 + 3.0 * etasq * (8.0 + etasq)))
        satrec.cc1   <- satrec.bstar * cc2
        cc3   <- 0.0;
        if (satrec.ecco > 1.0e-4) then
            cc3 <- -2.0 * coef * tsi * gravConsts.j3oj2 * satrec.no * initlResult.sinio / satrec.ecco
        satrec.x1mth2 <- 1.0 - initlResult.cosio2
        satrec.cc4    <- 2.0* satrec.no * coef1 * initlResult.ao * initlResult.omeosq *
                            (
                                satrec.eta * (2.0 + 0.5 * etasq) + satrec.ecco *
                                    (0.5 + 2.0 * etasq) - gravConsts.j2 * tsi / (initlResult.ao * psisq) *
                                    (-3.0 * satrec.con41 * (1.0 - 2.0 * eeta + etasq *
                                        (1.5 - 0.5 * eeta)) + 0.75 * satrec.x1mth2 *
                                        (2.0 * etasq - eeta * (1.0 + etasq)) * cos(2.0 * satrec.argpo))
                            )
        satrec.cc5 <- 2.0 * coef1 * initlResult.ao * initlResult.omeosq * (1.0 + 2.75 *
                    (etasq + eeta) + eeta * etasq)
        cosio4 <- initlResult.cosio2 ** 2.
        temp1  <- 1.5 * gravConsts.j2 * pinvsq * satrec.no
        temp2  <- 0.5 * temp1 * gravConsts.j2 * pinvsq
        temp3  <- -0.46875 * gravConsts.j4 * pinvsq * pinvsq * satrec.no
        satrec.mdot     <- satrec.no + 0.5 * temp1 * initlResult.rteosq * satrec.con41 + 0.0625 *
                        temp2 * initlResult.rteosq * (13.0 - 78.0 * initlResult.cosio2 + 137.0 * cosio4)
        satrec.argpdot  <- -0.5 * temp1 * initlResult.con42 + 0.0625 * temp2 *
                            (7.0 - 114.0 * initlResult.cosio2 + 395.0 * cosio4) +
                            temp3 * (3.0 - 36.0 * initlResult.cosio2 + 49.0 * cosio4)
        xhdot1            <- -temp1 * initlResult.cosio
        satrec.nodedot <- xhdot1 + (0.5 * temp2 * (4.0 - 19.0 * initlResult.cosio2) +
                            2.0 * temp3 * (3.0 - 7.0 * initlResult.cosio2)) * initlResult.cosio
        xpidot            <-  satrec.argpdot+ satrec.nodedot
        satrec.omgcof   <- satrec.bstar * cc3 * cos(satrec.argpo)
        satrec.xmcof    <- 0.0
        if (satrec.ecco > 1.0e-4) then
            satrec.xmcof <- -x2o3 * coef * satrec.bstar / eeta
        satrec.nodecf <- 3.5 * initlResult.omeosq * xhdot1 * satrec.cc1
        satrec.t2cof   <- 1.5 * satrec.cc1
        // sgp4fix for divide by zero with xinco <- 180 deg
        if (Math.Abs(initlResult.cosio+1.0) > 1.5e-12) then
            satrec.xlcof <- -0.25 * gravConsts.j3oj2 * initlResult.sinio * (3.0 + 5.0 * initlResult.cosio) / (1.0 + initlResult.cosio)
        else
            satrec.xlcof <- -0.25 * gravConsts.j3oj2 * initlResult.sinio * (3.0 + 5.0 * initlResult.cosio) / temp4
        satrec.aycof   <- -0.5 * gravConsts.j3oj2 * initlResult.sinio
        // sgp4fix use multiply for speed instead of pow
        delmotemp <- 1.0 + satrec.eta * cos(satrec.mo)
        satrec.delmo   <- delmotemp * delmotemp * delmotemp
        satrec.sinmao  <- sin(satrec.mo)
        satrec.x7thm1  <- 7.0 * initlResult.cosio2 - 1.0;

        // Deep space initialization:
        if ((2.*PI / satrec.no) >= 225.0) then
            satrec.method' <- 'd'
            satrec.isimp  <- 1s
            tc    <-  0.0
            inclm <- satrec.inclo;

            dscom epoch satrec.ecco satrec.argpo tc satrec.inclo satrec.nodeo
                  satrec.no &snodm &cnodm &sinim &cosim &sinomm &cosomm
                  &day &satrec.e3 &satrec.ee2 &em &emsq &gam
                  &satrec.peo &satrec.pgho &satrec.pho &satrec.pinco
                  &satrec.plo &rtemsq &satrec.se2 &satrec.se3
                  &satrec.sgh2 &satrec.sgh3 &satrec.sgh4
                  &satrec.sh2 &satrec.sh3 &satrec.si2 &satrec.si3
                  &satrec.sl2 &satrec.sl3 &satrec.sl4 &s1 &s2 &s3 &s4 &s5
                  &s6 &s7 &ss1 &ss2 &ss3 &ss4 &ss5 &ss6 &ss7 &sz1 &sz2 &sz3
                  &sz11 &sz12 &sz13 &sz21 &sz22 &sz23 &sz31 &sz32 &sz33
                  &satrec.xgh2 &satrec.xgh3 &satrec.xgh4 &satrec.xh2
                  &satrec.xh3 &satrec.xi2 &satrec.xi3 &satrec.xl2
                  &satrec.xl3 &satrec.xl4 &nm &z1 &z2 &z3 &z11
                  &z12 &z13 &z21 &z22 &z23 &z31 &z32 &z33
                  &satrec.zmol &satrec.zmos
            dpper satrec.e3 satrec.ee2 satrec.peo satrec.pgho
                  satrec.pho satrec.pinco satrec.plo satrec.se2
                  satrec.se3 satrec.sgh2 satrec.sgh3 satrec.sgh4
                  satrec.sh2 satrec.sh3 satrec.si2  satrec.si3
                  satrec.sl2 satrec.sl3 satrec.sl4  satrec.t
                  satrec.xgh2 satrec.xgh3 satrec.xgh4 satrec.xh2
                  satrec.xh3 satrec.xi2 satrec.xi3  satrec.xl2
                  satrec.xl3 satrec.xl4 satrec.zmol satrec.zmos inclm satrec.init
                  &(satrec.ecco) &(satrec.inclo) &(satrec.nodeo) &(satrec.argpo) &(satrec.mo)
                  satrec.operationmode

            argpm  <- 0.0
            nodem  <- 0.0
            mm     <- 0.0

            dsinit whichconst 
                   cosim emsq satrec.argpo s1 s2 s3 s4 s5 sinim ss1 ss2 ss3 ss4 
                   ss5 sz1 sz3 sz11 sz13 sz21 sz23 sz31 sz33 satrec.t tc 
                   satrec.gsto satrec.mo satrec.mdot satrec.no satrec.nodeo 
                   satrec.nodedot xpidot z1 z3 z11 z13 z21 z23 z31 z33 
                   satrec.ecco initlResult.eccsq &em &argpm &inclm &mm &nm &nodem 
                   &(satrec.irez) &(satrec.atime) 
                   &(satrec.d2201) &(satrec.d2211) &(satrec.d3210) &(satrec.d3222) 
                   &(satrec.d4410) &(satrec.d4422) &(satrec.d5220) &(satrec.d5232) 
                   &(satrec.d5421) &(satrec.d5433) &(satrec.dedt) &(satrec.didt) 
                   &(satrec.dmdt) &dndt &(satrec.dnodt) &(satrec.domdt) 
                   &(satrec.del1) &(satrec.del2) &(satrec.del3) &(satrec.xfact) 
                   &(satrec.xlamo) &(satrec.xli) &(satrec.xni)

    // Set variables if not deep space:
    if (satrec.isimp <> 1s) then
        cc1sq          <- satrec.cc1 * satrec.cc1
        satrec.d2    <- 4.0 * initlResult.ao * tsi * cc1sq
        temp           <- satrec.d2 * tsi * satrec.cc1 / 3.0
        satrec.d3    <- (17.0 * initlResult.ao + sfour) * temp
        satrec.d4    <- 0.5 * temp * initlResult.ao * tsi * (221.0 * initlResult.ao + 31.0 * sfour) *
                        satrec.cc1
        satrec.t3cof <- satrec.d2 + 2.0 * cc1sq
        satrec.t4cof <- 0.25 * (3.0 * satrec.d3 + satrec.cc1 *
                        (12.0 * satrec.d2 + 10.0 * cc1sq))
        satrec.t5cof <- 0.2 * (3.0 * satrec.d4 +
                        12.0 * satrec.cc1 * satrec.d3 +
                        6.0 * satrec.d2 * satrec.d2 +
                        15.0 * cc1sq * (2.0 * satrec.d2 + cc1sq))

    // Finally propogate to zero epoch to initialize all others:
    let ok, r, v = sgp4 whichconst satrec 0.0 

    satrec.init <- 'n'

    ok
    
