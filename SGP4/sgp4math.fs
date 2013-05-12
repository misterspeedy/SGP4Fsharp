module Sgp4Math

open System

/// Calculates the floating-point modulus of the given numerator and denominator.
let fmod (numerator : double) (denominator : double) = 
    let quotient = numerator / denominator |> int |> double
    numerator - (denominator * quotient)

/// Return the sign (-1.0 or 1.0) for the input.  
///
/// NB. Differs from Microsoft.FSharp.Core.Operators.sign 
/// as sgn returns a floating point number, and returns 1.0
/// for an input of 0.0 (core version returns 0 for input
/// of 0.0)
let sgn x =
    if (x < 0.) then -1.0 else 1.0

/// Returns the magnitude of a vector in 3D space, using
/// Pythagorus' Law.
let mag (x : double[]) =
    x.[0] ** 2. + x.[1] ** 2. + x.[2] ** 2. |> sqrt

/// Returns the cross-product of two vectors.
/// (Inputs must both have exactly 3 elements.)
let cross (vec1 : double[]) (vec2 : double[]) =

    if vec1.Length <> 3 || vec2.Length <> 3 then
        raise (ArgumentException("Input vectors must have length 3."))

    [| 
       vec1.[1]*vec2.[2] - vec1.[2]*vec2.[1]
       vec1.[2]*vec2.[0] - vec1.[0]*vec2.[2]
       vec1.[0]*vec2.[1] - vec1.[1]*vec2.[0]
    |]

/// Returns the dot-product of two vectors.
/// (Inputs must both have exactly 3 elements.)
let dot (x : double[]) (y : double[]) =

    if x.Length <> 3 || y.Length <> 3 then
        raise (ArgumentException("Input vectors must have length 3."))

    x.[0]*y.[0] + x.[1]*y.[1] + x.[2]*y.[2]