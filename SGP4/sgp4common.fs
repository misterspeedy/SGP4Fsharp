module sgp4common

let SGP4Version = "SGP4 Version 2013-03-18 (FSharp)"

// TODO repeated - move to own unit
type GravConstType =
    | Wgs72old
    | Wgs72
    | Wgs84

// May differ from System.Math.PI
let PI = 3.14159265358979323846

type ElSetRec = 
    {
        mutable satnum        : int 
        mutable epochyr       : int16
        mutable epochtynumrev : int16
        mutable error         : int16
        mutable operationmode : char
        mutable init          : char
        mutable method'       : char
        // Near Earth:
        mutable isimp         : int16
        mutable aycof         : double
        mutable con41         : double
        mutable cc1           : double
        mutable cc4           : double
        mutable cc5           : double
        mutable d2            : double
        mutable d3            : double
        mutable d4            : double
        mutable delmo         : double
        mutable eta           : double
        mutable argpdot       : double
        mutable omgcof        : double
        mutable sinmao        : double
        mutable t             : double
        mutable t2cof         : double
        mutable t3cof         : double
        mutable t4cof         : double
        mutable t5cof         : double
        mutable x1mth2        : double
        mutable x7thm1        : double
        mutable mdot          : double
        mutable nodedot       : double
        mutable xlcof         : double
        mutable xmcof         : double
        mutable nodecf        : double
        // Deep Space:
        mutable irez          : int16
        mutable d2201         : double
        mutable d2211         : double
        mutable d3210         : double
        mutable d3222         : double
        mutable d4410         : double
        mutable d4422         : double
        mutable d5220         : double
        mutable d5232         : double
        mutable d5421         : double
        mutable d5433         : double
        mutable dedt          : double
        mutable del1          : double
        mutable del2          : double
        mutable del3          : double
        mutable didt          : double
        mutable dmdt          : double
        mutable dnodt         : double
        mutable domdt         : double
        mutable e3            : double
        mutable ee2           : double
        mutable peo           : double
        mutable pgho          : double
        mutable pho           : double
        mutable pinco         : double
        mutable plo           : double
        mutable se2           : double
        mutable se3           : double
        mutable sgh2          : double
        mutable sgh3          : double
        mutable sgh4          : double
        mutable sh2           : double
        mutable sh3           : double
        mutable si2           : double
        mutable si3           : double
        mutable sl2           : double
        mutable sl3           : double
        mutable sl4           : double
        mutable gsto          : double
        mutable xfact         : double
        mutable xgh2          : double
        mutable xgh3          : double
        mutable xgh4          : double
        mutable xh2           : double
        mutable xh3           : double
        mutable xi2           : double
        mutable xi3           : double
        mutable xl2           : double
        mutable xl3           : double
        mutable xl4           : double
        mutable xlamo         : double
        mutable zmol          : double
        mutable zmos          : double
        mutable atime         : double
        mutable xli           : double
        mutable xni           : double
        mutable a             : double
        mutable altp          : double
        mutable alta          : double
        mutable epochdays     : double
        mutable jdsatepoch    : double
        mutable nddot         : double
        mutable ndot          : double
        mutable bstar         : double
        mutable rcse          : double
        mutable inclo         : double
        mutable nodeo         : double
        mutable ecco          : double
        mutable argpo         : double
        mutable mo            : double
        mutable no            : double
    }

