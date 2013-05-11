module Sgp4Constants

// Versioning:

let SGP4Version = "SGP4 Version 2013-03-18 (FSharp)"

// Angles:

// May differ from System.Math.PI
let PI = 3.14159265358979323846

let twopi = 2.0 * PI
let halfpi = 0.5 * PI
let rad = 180.0 / PI
let deg2rad = PI / 180.0

// Magic numbers:

//let small  = 0.00000001
//let undefined = 999999.1
//let infinite  = 999999.9

// Gravity:

type GravConstType =
    | Wgs72old
    | Wgs72
    | Wgs84

// Orbit types:

//type OrbitType =
//    | EllipticalInclined
//    | CircularEquatorial
//    | CircularInclined
//    | EllipticalParabolicHyperbolicEquatorial
