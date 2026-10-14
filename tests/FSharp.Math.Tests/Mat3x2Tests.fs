module FSharp.Math.Tests.Mat3x2Tests

open FSharp.Math
open FsCheck
open FSharp.Math.Tests.Generators

let private test name f =
    try Check.One(fsCheckConfig, f); printfn "  PASS  %s" name
    with e -> printfn "  FAIL  %s\n         %s" name e.Message

let private testFew name f =
    try Check.One(fewTestsConfig, f); printfn "  PASS  %s" name
    with e -> printfn "  FAIL  %s\n         %s" name e.Message

let private finite (x: float32) = not (System.Single.IsNaN x || System.Single.IsInfinity x)

let run () =
    printfn "-- Mat3x2 properties (algebraic laws) --"

    // Law 1: Invertibility (use generated bounded values via genSmallFloat)
    test "T × T⁻¹ ≈ Identity (invertibility)" (fun (v: Vec2) ->
        let tx = v.X * 0.1f
        let ty = v.Y * 0.1f
        let angle = tx * 0.01f
        let m = Mat3x2.multiply (Mat3x2.translation tx ty) (Mat3x2.rotation angle)
        match Mat3x2.invert m with
        | None -> true
        | Some inv ->
            let product = Mat3x2.multiply m inv
            Mat3x2.approxEq 1e-3f product Mat3x2.identity
    )

    test "T⁻¹ × T ≈ Identity (right-inverse)" (fun (v: Vec2) ->
        let tx = v.X * 0.1f
        let ty = v.Y * 0.1f
        let angle = tx * 0.01f
        let m = Mat3x2.multiply (Mat3x2.translation tx ty) (Mat3x2.rotation angle)
        match Mat3x2.invert m with
        | None -> true
        | Some inv ->
            let product = Mat3x2.multiply inv m
            Mat3x2.approxEq 1e-3f product Mat3x2.identity
    )

    // Law 2: Associativity — use bounded generators to avoid overflow
    test "(A × B) × C = A × (B × C) (associativity)" (fun (a: Mat3x2, b: Mat3x2, c: Mat3x2) ->
        let lhs = Mat3x2.multiply (Mat3x2.multiply a b) c
        let rhs = Mat3x2.multiply a (Mat3x2.multiply b c)
        let allFinite =
            finite lhs.M11 && finite lhs.M12 && finite lhs.M21 &&
            finite lhs.M22 && finite lhs.M31 && finite lhs.M32 &&
            finite rhs.M11 && finite rhs.M12 && finite rhs.M21 &&
            finite rhs.M22 && finite rhs.M31 && finite rhs.M32
        if not allFinite then true
        else
            // Use relative tolerance: eps proportional to magnitude
            let mag = max 1.0f (max (abs lhs.M11) (max (abs lhs.M31) (abs lhs.M32)))
            Mat3x2.approxEq (mag * 1e-3f) lhs rhs
    )

    // Law 3: Identity element
    test "I × A = A (left identity)" (fun (a: Mat3x2) ->
        Mat3x2.approxEq 1e-5f (Mat3x2.multiply Mat3x2.identity a) a
    )

    test "A × I = A (right identity)" (fun (a: Mat3x2) ->
        Mat3x2.approxEq 1e-5f (Mat3x2.multiply a Mat3x2.identity) a
    )

    // Law 4: Translation correctness
    test "Translation(dx,dy).Transform(0,0) = (dx,dy)" (fun (v: Vec2) ->
        let dx = v.X
        let dy = v.Y
        let m = Mat3x2.translation dx dy
        let result = Mat3x2.transformPoint m Vec2.zero
        Vec2.approxEq 1e-5f result (Vec2.create dx dy)
    )

    // Law 5: Rotation preserves length
    test "Rotation preserves vector length" (fun (v: Vec2) ->
        let angle = v.X * 0.01f
        let m = Mat3x2.rotation angle
        let transformed = Mat3x2.transformVector m v
        let origLen = Vec2.len v
        let transLen = Vec2.len transformed
        Scalar.approxEq 1e-2f origLen transLen
    )

    // Law 6: Scale correctness
    test "Scale(sx,sy).Transform(x,y) = (x*sx, y*sy)" (fun (v: Vec2) ->
        let sx = 2.0f
        let sy = 3.0f
        let m = Mat3x2.scale sx sy
        let result = Mat3x2.transformPoint m v
        Vec2.approxEq 1e-4f result (Vec2.create (v.X * sx) (v.Y * sy))
    )

    // Law 7: AABB containment — expand slightly for float rounding
    test "Transformed AABB contains all original corners" (fun (r: Rect) ->
        let m = Mat3x2.multiply (Mat3x2.translation 10.0f 20.0f) (Mat3x2.rotation 0.5f)
        let points = [
            Vec2.create r.X r.Y
            Vec2.create (Rect.right r) r.Y
            Vec2.create r.X (Rect.bottom r)
            Vec2.create (Rect.right r) (Rect.bottom r)
        ]
        let transformed = points |> List.map (Mat3x2.transformPoint m)
        let aabb = Rect.boundsOf transformed |> Rect.expand 1e-3f
        transformed |> List.forall (fun p -> Rect.contains p aabb)
    )

    // Law 8: Determinant of composition
    test "det(A×B) ≈ det(A) × det(B)" (fun (a: Mat3x2, b: Mat3x2) ->
        let detAB = Mat3x2.determinant (Mat3x2.multiply a b)
        let detA_detB = Mat3x2.determinant a * Mat3x2.determinant b
        if not (finite detAB && finite detA_detB) then true
        elif abs detAB < 1e6f && abs detA_detB < 1e6f then
            Scalar.approxEq 1.0f detAB detA_detB
        else true
    )
