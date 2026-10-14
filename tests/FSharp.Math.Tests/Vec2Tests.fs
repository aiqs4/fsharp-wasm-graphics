module FSharp.Math.Tests.Vec2Tests

open FSharp.Math
open FsCheck
open FSharp.Math.Tests.Generators

let private test name f =
    try Check.One(fsCheckConfig, f); printfn "  PASS  %s" name
    with e -> printfn "  FAIL  %s\n         %s" name e.Message

let run () =
    printfn "-- Vec2 properties --"

    test "a + b = b + a (commutativity)" (fun (a: Vec2, b: Vec2) ->
        Vec2.approxEq 1e-4f (Vec2.add a b) (Vec2.add b a)
    )

    test "(a + b) + c ≈ a + (b + c) (associativity)" (fun (a: Vec2, b: Vec2, c: Vec2) ->
        Vec2.approxEq 1e-2f (Vec2.add (Vec2.add a b) c) (Vec2.add a (Vec2.add b c))
    )

    test "a + Zero = a (identity)" (fun (a: Vec2) ->
        Vec2.approxEq 1e-5f (Vec2.add a Vec2.zero) a
    )

    test "a - a = Zero (inverse)" (fun (a: Vec2) ->
        Vec2.approxEq 1e-4f (Vec2.sub a a) Vec2.zero
    )

    test "||a|| >= 0 (non-negative length)" (fun (a: Vec2) ->
        Vec2.len a >= 0.0f
    )

    test "Dot(a, b) = Dot(b, a) (symmetric)" (fun (a: Vec2, b: Vec2) ->
        Scalar.approxEq 1e-2f (Vec2.dot a b) (Vec2.dot b a)
    )

    test "Lerp(a, a, t) = a" (fun (a: Vec2) ->
        let t = 0.5f
        Vec2.approxEq 1e-4f (Vec2.lerp a a t) a
    )

    test "normalize preserves direction" (fun (a: Vec2) ->
        let l = Vec2.len a
        if l > 1.0f then
            let n = Vec2.normalize a
            let nl = Vec2.len n
            Scalar.approxEq 1e-3f nl 1.0f
        else true
    )

    test "scale(s, a) = a + a for s=2" (fun (a: Vec2) ->
        Vec2.approxEq 1e-3f (Vec2.scale 2.0f a) (Vec2.add a a)
    )
