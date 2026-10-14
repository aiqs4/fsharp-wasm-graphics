module FSharp.Math.Scalar

/// float32 pi
let [<Literal>] Pi    = 3.14159265358979323846f
let [<Literal>] TwoPi = 6.28318530717958647692f
let [<Literal>] HalfPi = 1.57079632679489661923f
let [<Literal>] Eps   = 1e-6f

let inline clamp (lo: float32) (hi: float32) (x: float32) = 
    if x < lo then lo elif x > hi then hi else x

let inline lerp (a: float32) (b: float32) (t: float32) = a + (b - a) * t

let inline smoothStep (lo: float32) (hi: float32) (x: float32) =
    let t = clamp 0.0f 1.0f ((x - lo) / (hi - lo))
    t * t * (3.0f - 2.0f * t)

let inline approxEq (eps: float32) (a: float32) (b: float32) = abs (a - b) < eps

// sin/cos/sqrt — delegate to System.MathF on .NET; compiler team fills in WASM intrinsics later
let inline sin (x: float32) = System.MathF.Sin(x)
let inline cos (x: float32) = System.MathF.Cos(x)
let inline sqrt (x: float32) = System.MathF.Sqrt(x)
let inline atan2 (y: float32) (x: float32) = System.MathF.Atan2(y, x)
let inline floor (x: float32) = System.MathF.Floor(x)
let inline ceil (x: float32) = System.MathF.Ceiling(x)
let inline round (x: float32) = System.MathF.Round(x)
let inline abs' (x: float32) = if x < 0.0f then -x else x
let inline min' (a: float32) (b: float32) = if a < b then a else b
let inline max' (a: float32) (b: float32) = if a > b then a else b
let inline sign' (x: float32) = if x > 0.0f then 1.0f elif x < 0.0f then -1.0f else 0.0f

let inline toRadians (degrees: float32) = degrees * Pi / 180.0f
let inline toDegrees (radians: float32) = radians * 180.0f / Pi
