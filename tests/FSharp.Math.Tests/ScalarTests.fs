module FSharp.Math.Tests.ScalarTests

open FSharp.Math
open FsCheck
open FSharp.Math.Tests.Generators

let private finite (x: float32) = not (System.Single.IsNaN x || System.Single.IsInfinity x)

let run () =
    printfn "-- Scalar properties --"

    let test name f =
        try Check.One(fsCheckConfig, f); printfn "  PASS  %s" name
        with e -> printfn "  FAIL  %s\n         %s" name e.Message

    test "clamp returns value in [lo, hi]" (fun (x: float32) ->
        if not (finite x) then true
        else
            let lo = -100.0f
            let hi = 100.0f
            let r = Scalar.clamp lo hi x
            r >= lo && r <= hi
    )

    test "lerp(a, b, 0) = a" (fun (a: float32, b: float32) ->
        if not (finite a && finite b) then true
        else Scalar.approxEq 1e-5f (Scalar.lerp a b 0.0f) a
    )

    test "lerp(a, b, 1) = b" (fun (a: float32, b: float32) ->
        if not (finite a && finite b) then true
        elif abs a > 1e30f || abs b > 1e30f then true
        else Scalar.approxEq 1e-3f (Scalar.lerp a b 1.0f) b
    )

    test "smoothStep(lo, hi, lo) = 0" (fun () ->
        Scalar.approxEq 1e-5f (Scalar.smoothStep 0.0f 1.0f 0.0f) 0.0f
    )

    test "smoothStep(lo, hi, hi) = 1" (fun () ->
        Scalar.approxEq 1e-5f (Scalar.smoothStep 0.0f 1.0f 1.0f) 1.0f
    )
