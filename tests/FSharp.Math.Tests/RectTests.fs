module FSharp.Math.Tests.RectTests

open FSharp.Math
open FsCheck
open FSharp.Math.Tests.Generators

let private test name f =
    try Check.One(fsCheckConfig, f); printfn "  PASS  %s" name
    with e -> printfn "  FAIL  %s\n         %s" name e.Message

let run () =
    printfn "-- Rect properties --"

    test "Center is always inside rect" (fun (r: Rect) ->
        let c = Rect.center r
        Rect.contains c r
    )

    test "Intersect with self = identity" (fun (r: Rect) ->
        let i = Rect.intersect r r
        Rect.approxEq 1e-4f i r
    )

    test "Intersect is commutative" (fun (a: Rect, b: Rect) ->
        Rect.approxEq 1e-4f (Rect.intersect a b) (Rect.intersect b a)
    )

    test "Union is commutative" (fun (a: Rect, b: Rect) ->
        Rect.approxEq 1e-4f (Rect.union a b) (Rect.union b a)
    )

    test "Union contains both inputs' centers" (fun (a: Rect, b: Rect) ->
        let u = Rect.union a b
        Rect.contains (Rect.center a) u && Rect.contains (Rect.center b) u
    )

    test "expand grows all edges" (fun (r: Rect) ->
        let amount = 5.0f
        let e = Rect.expand amount r
        e.W >= r.W && e.H >= r.H
    )

    test "boundsOf contains all input points" (fun (a: Vec2, b: Vec2, c: Vec2) ->
        let bounds = Rect.expand 1e-4f (Rect.boundsOf [a; b; c])
        Rect.contains a bounds && Rect.contains b bounds && Rect.contains c bounds
    )
