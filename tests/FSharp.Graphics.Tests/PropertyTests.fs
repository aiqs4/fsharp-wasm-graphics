/// Property-based tests for FSharp.Graphics using FsCheck.
///
/// These tests verify the algebraic laws and mathematical invariants of the
/// scene graph library:
///   - Matrix3x2 invertibility, associativity, identity
///   - Local-to-world transform consistency
///   - SDF boundary correctness
///   - Hit-testing bounding box inclusion
///   - Layout conservation of space and monotonicity
///   - Vec2 algebraic laws
///   - Color compositing and interpolation laws
module FSharp.Graphics.Tests.PropertyTests

open FSharp.Graphics
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.Layout
open GlyphAuro
open FsCheck

// Aliases for FsCheck.FSharp module functions
module FsGen = FsCheck.FSharp.Gen
// Import gen { } computation expression builder
let private gen = FsCheck.FSharp.GenBuilder.gen

// ─────────────────────────────────────────────────────────────────────────────
// FsCheck generators for domain types
// ─────────────────────────────────────────────────────────────────────────────

/// Bounded float32 generator: [-range, range], finite.
let private genFloat32 (range: float32) =
    FsGen.choose(-10000, 10000)
    |> FsGen.map (fun i -> float32 i / 10000.0f * range)

/// Positive bounded float32 generator: [minVal, range].
let private genPositiveFloat32 (minVal: float32) (range: float32) =
    FsGen.choose(int (minVal * 100.0f), int (range * 100.0f))
    |> FsGen.map (fun i -> float32 i / 100.0f)

type Generators =
    /// Generate Vec2 with finite, bounded components
    static member Vec2() : Arbitrary<Vec2> =
        { new Arbitrary<Vec2>() with
            override _.Generator =
                gen {
                    let! x = genFloat32 500.0f
                    let! y = genFloat32 500.0f
                    return Vec2.Create(x, y)
                } }

    /// Generate invertible Matrix3x2 (non-degenerate determinant)
    static member Matrix3x2() : Arbitrary<Matrix3x2> =
        { new Arbitrary<Matrix3x2>() with
            override _.Generator =
                FsGen.oneof [
                    // Translation
                    gen {
                        let! dx = genFloat32 500.0f
                        let! dy = genFloat32 500.0f
                        return Matrix3x2.Translation(dx, dy)
                    }
                    // Rotation
                    gen {
                        let! a = FsGen.choose(0, 6283)
                        let angle = float32 a / 1000.0f
                        return Matrix3x2.Rotation(angle)
                    }
                    // Scale (non-zero)
                    gen {
                        let! sx = FsGen.choose(1, 100) |> FsGen.map (fun i -> float32 i / 10.0f)
                        let! sy = FsGen.choose(1, 100) |> FsGen.map (fun i -> float32 i / 10.0f)
                        return Matrix3x2.Scale(sx, sy)
                    }
                    // Composite: translation * rotation
                    gen {
                        let! dx = genFloat32 100.0f
                        let! dy = genFloat32 100.0f
                        let! a = FsGen.choose(0, 6283)
                        let angle = float32 a / 1000.0f
                        return Matrix3x2.Translation(dx, dy) * Matrix3x2.Rotation(angle)
                    }
                ] }

    /// Generate Rect with positive size
    static member Rect() : Arbitrary<Rect> =
        { new Arbitrary<Rect>() with
            override _.Generator =
                gen {
                    let! x = genFloat32 200.0f
                    let! y = genFloat32 200.0f
                    let! w = genPositiveFloat32 1.0f 500.0f
                    let! h = genPositiveFloat32 1.0f 500.0f
                    return Rect.Create(x, y, w, h)
                } }

    /// Generate Color with valid [0,1] components
    static member Color() : Arbitrary<Color> =
        { new Arbitrary<Color>() with
            override _.Generator =
                gen {
                    let! r = FsGen.choose(0, 255) |> FsGen.map (fun v -> float32 v / 255.0f)
                    let! g = FsGen.choose(0, 255) |> FsGen.map (fun v -> float32 v / 255.0f)
                    let! b = FsGen.choose(0, 255) |> FsGen.map (fun v -> float32 v / 255.0f)
                    let! a = FsGen.choose(0, 255) |> FsGen.map (fun v -> float32 v / 255.0f)
                    return Color.Create(r, g, b, a)
                } }

let fsCheckConfig =
    Config.QuickThrowOnFailure
        .WithMaxTest(200)
        .WithArbitrary([ typeof<Generators> ])

let fewTestsConfig =
    Config.QuickThrowOnFailure
        .WithMaxTest(50)
        .WithArbitrary([ typeof<Generators> ])

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

let mutable private passed = 0
let mutable private failed = 0

let private matClose (a: Matrix3x2) (b: Matrix3x2) (eps: float32) =
    abs (a.M11 - b.M11) <= eps &&
    abs (a.M12 - b.M12) <= eps &&
    abs (a.M21 - b.M21) <= eps &&
    abs (a.M22 - b.M22) <= eps &&
    abs (a.M31 - b.M31) <= eps &&
    abs (a.M32 - b.M32) <= eps

let private vecClose (a: Vec2) (b: Vec2) (eps: float32) =
    abs (a.X - b.X) <= eps && abs (a.Y - b.Y) <= eps

let private prop name (p: unit -> unit) =
    try
        p ()
        passed <- passed + 1
        printfn "  PASS  %s" name
    with ex ->
        failed <- failed + 1
        printfn "  FAIL  %s\n         %s" name (ex.Message.Split('\n').[0])

// ─────────────────────────────────────────────────────────────────────────────
// 1. Matrix3x2 algebraic laws
// ─────────────────────────────────────────────────────────────────────────────

let testMatrixProperties () =
    prop "Matrix3x2: T × T⁻¹ ≈ Identity (invertibility)" (fun () ->
        Check.One(fsCheckConfig, fun (m: Matrix3x2) ->
            match m.Invert() with
            | ValueSome inv ->
                matClose (m * inv) Matrix3x2.Identity 1e-3f
            | ValueNone -> true
        )
    )

    prop "Matrix3x2: T⁻¹ × T ≈ Identity (right-inverse)" (fun () ->
        Check.One(fsCheckConfig, fun (m: Matrix3x2) ->
            match m.Invert() with
            | ValueSome inv ->
                matClose (inv * m) Matrix3x2.Identity 1e-3f
            | ValueNone -> true
        )
    )

    prop "Matrix3x2: (A × B) × C = A × (B × C) (associativity)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Matrix3x2, b: Matrix3x2, c: Matrix3x2) ->
            matClose ((a * b) * c) (a * (b * c)) 1e-2f
        )
    )

    prop "Matrix3x2: I × A = A (left identity)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Matrix3x2) ->
            matClose (Matrix3x2.Identity * a) a 1e-5f
        )
    )

    prop "Matrix3x2: A × I = A (right identity)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Matrix3x2) ->
            matClose (a * Matrix3x2.Identity) a 1e-5f
        )
    )

    prop "Matrix3x2: Translation(dx,dy).Transform(0,0) = (dx,dy)" (fun () ->
        Check.One(fsCheckConfig, fun (v: Vec2) ->
            let m = Matrix3x2.Translation(v.X, v.Y)
            let p = m.Transform(Vec2.Zero)
            vecClose p v 1e-5f
        )
    )

    prop "Matrix3x2: Rotation preserves vector length" (fun () ->
        Check.One(fsCheckConfig, fun (v: Vec2) ->
            let angle = 1.2345f
            let m = Matrix3x2.Rotation(angle)
            let rotated = m.Transform(v)
            abs (rotated.Length - v.Length) <= 1e-3f * (v.Length + 1.0f)
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// 2. Vec2 algebraic laws
// ─────────────────────────────────────────────────────────────────────────────

let testVec2Properties () =
    prop "Vec2: a + b = b + a (commutativity)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Vec2, b: Vec2) ->
            vecClose (a + b) (b + a) 1e-5f
        )
    )

    prop "Vec2: (a + b) + c ≈ a + (b + c) (associativity)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Vec2, b: Vec2, c: Vec2) ->
            vecClose ((a + b) + c) (a + (b + c)) 1e-3f
        )
    )

    prop "Vec2: a + Zero = a (additive identity)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Vec2) ->
            vecClose (a + Vec2.Zero) a 1e-5f
        )
    )

    prop "Vec2: a - a = Zero (additive inverse)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Vec2) ->
            vecClose (a - a) Vec2.Zero 1e-5f
        )
    )

    prop "Vec2: ||a|| >= 0 (non-negative length)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Vec2) ->
            a.Length >= 0.0f
        )
    )

    prop "Vec2: Dot(a, b) = Dot(b, a) (symmetric dot product)" (fun () ->
        Check.One(fsCheckConfig, fun (a: Vec2, b: Vec2) ->
            abs (Vec2.Dot(a, b) - Vec2.Dot(b, a)) <= 1e-3f
        )
    )

    prop "Vec2: Lerp(a, a, t) = a" (fun () ->
        Check.One(fsCheckConfig, fun (a: Vec2) ->
            let t = 0.5f
            vecClose (Vec2.Lerp(a, a, t)) a 1e-5f
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// 3. Local-to-World transform consistency
// ─────────────────────────────────────────────────────────────────────────────

let testTransformConsistency () =
    prop "Nested transforms: (A*B).Transform(p) = B.Transform(A.Transform(p))" (fun () ->
        Check.One(fsCheckConfig, fun (parentT: Matrix3x2, childT: Matrix3x2) ->
            let localPoint = Vec2.Create(5.0f, 10.0f)
            let composed = parentT * childT
            let composedResult = composed.Transform(localPoint)
            // Row-vector convention: (A*B).T(p) = B.T(A.T(p))
            let stepwise = childT.Transform(parentT.Transform(localPoint))
            vecClose composedResult stepwise 1e-2f
        )
    )

    prop "Deeply nested transforms (5 levels) stay consistent" (fun () ->
        Check.One(fewTestsConfig, fun (transforms: Matrix3x2 list) ->
            let transforms = transforms |> List.truncate 5
            if transforms.IsEmpty then true
            else
                let point = Vec2.Create(1.0f, 1.0f)
                let composed = transforms |> List.fold (fun acc m -> acc * m) Matrix3x2.Identity
                let composedResult = composed.Transform(point)
                // Row-vector convention: fold (*) applies left-to-right, transform applies right-to-left
                let stepwiseResult =
                    transforms |> List.fold (fun p m -> m.Transform(p)) point
                vecClose composedResult stepwiseResult 0.5f
        )
    )

    prop "Translate then translate-back returns to origin" (fun () ->
        Check.One(fsCheckConfig, fun (v: Vec2) ->
            let dx = v.X |> max -200.0f |> min 200.0f
            let dy = v.Y |> max -200.0f |> min 200.0f
            let point = Vec2.Create(42.0f, 17.0f)
            let m = Matrix3x2.Translation(dx, dy)
            let minv = Matrix3x2.Translation(-dx, -dy)
            let result = minv.Transform(m.Transform(point))
            vecClose result point 1e-3f
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// 4. SDF boundary correctness
// ─────────────────────────────────────────────────────────────────────────────

let testSdfProperties () =
    prop "CircleSdf: center point is inside (negative distance)" (fun () ->
        Check.One(fsCheckConfig, fun (center: Vec2, r: Rect) ->
            let cx = center.X |> max -100.0f |> min 100.0f
            let cy = center.Y |> max -100.0f |> min 100.0f
            let radius = r.W |> max 1.0f |> min 100.0f
            let shape = FSharp.Graphics.Primitives.CircleSdf(Vec2.Create(cx, cy), radius) :> ISdfShape
            let d = shape.Sdf(Vec2.Create(cx, cy))
            d <= 0.0f
        )
    )

    prop "CircleSdf: far point is outside (positive distance)" (fun () ->
        Check.One(fsCheckConfig, fun (center: Vec2, r: Rect) ->
            let cx = center.X |> max -100.0f |> min 100.0f
            let cy = center.Y |> max -100.0f |> min 100.0f
            let radius = r.W |> max 1.0f |> min 100.0f
            let shape = FSharp.Graphics.Primitives.CircleSdf(Vec2.Create(cx, cy), radius) :> ISdfShape
            let farPoint = Vec2.Create(cx + radius * 2.0f + 10.0f, cy)
            let d = shape.Sdf(farPoint)
            d > 0.0f
        )
    )

    prop "RectSdf: center point is inside" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let shape = FSharp.Graphics.Primitives.RectSdf(r, 0.0f, 0.0f, 0.0f, 0.0f) :> ISdfShape
            let center = r.Center
            let d = shape.Sdf(center)
            d <= 0.0f
        )
    )

    prop "RectSdf: bounds contain the shape" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let shape = FSharp.Graphics.Primitives.RectSdf(r, 0.0f, 0.0f, 0.0f, 0.0f) :> ISdfShape
            let b = shape.Bounds
            b.W >= r.W - 0.1f && b.H >= r.H - 0.1f
        )
    )

    prop "SDF distance at boundary ≈ 0 for circle" (fun () ->
        Check.One(fsCheckConfig, fun (center: Vec2, r: Rect) ->
            let cx = center.X |> max -100.0f |> min 100.0f
            let cy = center.Y |> max -100.0f |> min 100.0f
            let radius = r.W |> max 1.0f |> min 100.0f
            let shape = FSharp.Graphics.Primitives.CircleSdf(Vec2.Create(cx, cy), radius) :> ISdfShape
            let boundaryPoint = Vec2.Create(cx + radius, cy)
            let d = shape.Sdf(boundaryPoint)
            abs d <= 0.01f
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// 5. Hit-testing & bounding boxes
// ─────────────────────────────────────────────────────────────────────────────

let testHitTestProperties () =
    prop "Rect.Contains: center is always inside" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            r.Contains(r.Center)
        )
    )

    prop "Rect.Intersect is commutative" (fun () ->
        Check.One(fsCheckConfig, fun (a: Rect, b: Rect) ->
            let i1 = Rect.Intersect a b
            let i2 = Rect.Intersect b a
            i1 = i2
        )
    )

    prop "Rect.Union is commutative" (fun () ->
        Check.One(fsCheckConfig, fun (a: Rect, b: Rect) ->
            let u1 = Rect.Union a b
            let u2 = Rect.Union b a
            u1 = u2
        )
    )

    prop "Rect.Union contains both input centers" (fun () ->
        Check.One(fsCheckConfig, fun (a: Rect, b: Rect) ->
            let u = Rect.Union a b
            u.Contains(a.Center) && u.Contains(b.Center)
        )
    )

    prop "Inclusion law: shape center inside bounding box" (fun () ->
        Check.One(fsCheckConfig, fun (center: Vec2, r: Rect) ->
            let cx = center.X |> max -50.0f |> min 50.0f
            let cy = center.Y |> max -50.0f |> min 50.0f
            let radius = r.W |> max 1.0f |> min 50.0f
            let shape = FSharp.Graphics.Primitives.CircleSdf(Vec2.Create(cx, cy), radius) :> ISdfShape
            let bounds = shape.Bounds
            bounds.Contains(Vec2.Create(cx, cy))
        )
    )

    prop "Zero-scale transform collapses to origin" (fun () ->
        Check.One(fsCheckConfig, fun (v: Vec2) ->
            let m = Matrix3x2.Scale(0.0f, 0.0f)
            let result = m.Transform(v)
            vecClose result Vec2.Zero 1e-5f
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// 6. Layout properties
// ─────────────────────────────────────────────────────────────────────────────

let private mkShape (w: float32) (h: float32) =
    Shape (Rectangle(Rect.Create(0.0f, 0.0f, w, h),
           CornerRadius.Zero, Solid Color.Red, Option.None, Option.None))

let testLayoutProperties () =
    prop "Layout: Empty measures to zero" (fun () ->
        Check.One(fsCheckConfig, fun (w: Rect) ->
            let available = Vec2.Create(w.W, w.H)
            let size = LayoutEngine.measure Empty available
            vecClose size Vec2.Zero 1e-5f
        )
    )

    prop "Layout: Shape measure = its bounds size" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let shape = Rectangle(r, CornerRadius.Zero, Solid Color.Red, Option.None, Option.None)
            let size = LayoutEngine.measure (Shape shape) (Vec2.Create(1000.0f, 1000.0f))
            abs (size.X - r.W) <= 0.1f && abs (size.Y - r.H) <= 0.1f
        )
    )

    prop "Layout: Padding adds insets to child size" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let pad = r.W |> max 1.0f |> min 50.0f
            let padded = Padding(Insets.All pad, mkShape 100.0f 50.0f)
            let size = LayoutEngine.measure padded (Vec2.Create(1000.0f, 1000.0f))
            abs (size.X - (100.0f + pad * 2.0f)) <= 0.5f &&
            abs (size.Y - (50.0f + pad * 2.0f)) <= 0.5f
        )
    )

    prop "Layout: VStack height ≥ sum of children heights" (fun () ->
        Check.One(fewTestsConfig, fun (count: int) ->
            let count = (abs count % 5) + 1
            let children =
                [ for i in 0..count-1 -> mkShape 50.0f (float32 (10 + i * 5)) ]
            let stack = Stack(Vertical, 0.0f, Start, children)
            let size = LayoutEngine.measure stack (Vec2.Create(1000.0f, 1000.0f))
            let sumH = children |> List.sumBy (fun c ->
                let s = LayoutEngine.measure c (Vec2.Create(1000.0f, 1000.0f))
                s.Y)
            size.Y >= sumH - 0.1f
        )
    )

    prop "Layout: HStack width ≥ sum of children widths" (fun () ->
        Check.One(fewTestsConfig, fun (count: int) ->
            let count = (abs count % 5) + 1
            let children =
                [ for i in 0..count-1 -> mkShape (float32 (20 + i * 10)) 30.0f ]
            let stack = Stack(Horizontal, 0.0f, Start, children)
            let size = LayoutEngine.measure stack (Vec2.Create(1000.0f, 1000.0f))
            let sumW = children |> List.sumBy (fun c ->
                let s = LayoutEngine.measure c (Vec2.Create(1000.0f, 1000.0f))
                s.X)
            size.X >= sumW - 0.1f
        )
    )

    prop "Layout: adding child never shrinks total (monotonicity)" (fun () ->
        Check.One(fewTestsConfig, fun (n: int) ->
            let n = (abs n % 4) + 1
            let available = Vec2.Create(1000.0f, 1000.0f)
            let child = mkShape 40.0f 20.0f
            let small = Stack(Vertical, 0.0f, Start, [ for _ in 1..n -> child ])
            let large = Stack(Vertical, 0.0f, Start, [ for _ in 1..n+1 -> child ])
            let s1 = LayoutEngine.measure small available
            let s2 = LayoutEngine.measure large available
            s2.Y >= s1.Y - 0.1f
        )
    )

    prop "Layout: single child fits within available width" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let w = r.W |> max 50.0f |> min 500.0f
            let available = Vec2.Create(w, 1000.0f)
            let size = LayoutEngine.measure (mkShape 30.0f 20.0f) available
            size.X <= w + 0.1f
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// 7. Diff algebraic laws
// ─────────────────────────────────────────────────────────────────────────────

let testDiffProperties () =
    prop "Diff: identical scene → NoChange" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let scene = mkShape r.W r.H
            match Diff.diff scene scene with
            | NoChange -> true
            | _ -> false
        )
    )

    prop "Diff: same-reference scene → NoChange" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let scene = Shape (Rectangle(r, CornerRadius.Zero, Solid Color.Red, Option.None, Option.None))
            match Diff.diff scene scene with
            | NoChange -> true
            | _ -> false
        )
    )

    prop "Diff: structurally different types → Replace" (fun () ->
        Check.One(fsCheckConfig, fun (r: Rect) ->
            let s1 = mkShape r.W r.H
            let s2 = Shape (Circle(Vec2.Create(50.0f, 50.0f), 20.0f, Solid Color.Blue, Option.None, Option.None))
            match Diff.diff s1 s2 with
            | Replace _ -> true
            | _ -> false
        )
    )

    prop "Diff: Empty to Empty → NoChange" (fun () ->
        Check.One(fsCheckConfig, fun (_: Vec2) ->
            match Diff.diff Empty Empty with
            | NoChange -> true
            | _ -> false
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// 8. Color algebraic properties
// ─────────────────────────────────────────────────────────────────────────────

let testColorProperties () =
    prop "Color: Over with Transparent fg leaves bg unchanged" (fun () ->
        Check.One(fsCheckConfig, fun (bg: Color) ->
            let result = Color.Over Color.Transparent bg
            abs (result.R - bg.R) <= 0.01f &&
            abs (result.G - bg.G) <= 0.01f &&
            abs (result.B - bg.B) <= 0.01f &&
            abs (result.A - bg.A) <= 0.01f
        )
    )

    prop "Color: Lerp(a, b, 0) = a" (fun () ->
        Check.One(fsCheckConfig, fun (a: Color, b: Color) ->
            let result = Color.Lerp(a, b, 0.0f)
            abs (result.R - a.R) <= 0.01f &&
            abs (result.G - a.G) <= 0.01f &&
            abs (result.B - a.B) <= 0.01f &&
            abs (result.A - a.A) <= 0.01f
        )
    )

    prop "Color: Lerp(a, b, 1) = b" (fun () ->
        Check.One(fsCheckConfig, fun (a: Color, b: Color) ->
            let result = Color.Lerp(a, b, 1.0f)
            abs (result.R - b.R) <= 0.01f &&
            abs (result.G - b.G) <= 0.01f &&
            abs (result.B - b.B) <= 0.01f &&
            abs (result.A - b.A) <= 0.01f
        )
    )

    prop "Color: ToRgba8 → FromRgba8 roundtrip (8-bit precision)" (fun () ->
        Check.One(fsCheckConfig, fun (c: Color) ->
            let packed = c.ToRgba8()
            let unpacked = Color.FromRgba8(packed)
            let repacked = unpacked.ToRgba8()
            packed = repacked
        )
    )


// ───────────────────────────────────────────────────────────────────────────────
// M5 — Easing algebraic laws (FsCheck)
// ───────────────────────────────────────────────────────────────────────────────

let testEasingProperties () =
    let genT = FsGen.choose(0, 1000) |> FsGen.map (fun i -> float32 i / 1000.0f)

    prop "Easing.Linear: f(0)=0, f(1)=1" (fun () ->
        Check.One(fsCheckConfig, fun () ->
            Easing.evaluate Linear 0.0f = 0.0f &&
            Easing.evaluate Linear 1.0f = 1.0f
        )
    )

    prop "Easing: non-spring functions stay within [0,1]" (fun () ->
        let easings = [| Linear; EaseIn; EaseOut; EaseInOut |]
        Check.One(
            fewTestsConfig,
            fun () ->
                gen {
                    let! t = genT
                    let! eIdx = FsGen.choose(0, 3)
                    return easings.[eIdx], t
                } |> FsGen.sample 50 |> Array.forall (fun (e, t) ->
                    let v = Easing.evaluate e t
                    v >= -0.001f && v <= 1.001f
                )
        )
    )

    prop "Easing: Spring output is bounded within [-0.5, 2.0] (intentional overshoot)" (fun () ->
        Check.One(
            fewTestsConfig,
            fun () ->
                gen {
                    let! t = genT
                    return t
                } |> FsGen.sample 50 |> Array.forall (fun t ->
                    let v = Easing.evaluate Spring t
                    v >= -0.5f && v <= 2.0f
                )
        )
    )

    prop "Easing: f(0) = 0 for all easing types" (fun () ->
        Check.One(fsCheckConfig, fun () ->
            [ Linear; EaseIn; EaseOut; EaseInOut; Spring ]
            |> List.forall (fun e -> abs (Easing.evaluate e 0.0f) < 0.001f)
        )
    )

    prop "Easing: f(1) = 1 for standard easing types" (fun () ->
        Check.One(fsCheckConfig, fun () ->
            [ Linear; EaseIn; EaseOut; EaseInOut ]
            |> List.forall (fun e -> abs (Easing.evaluate e 1.0f - 1.0f) < 0.001f)
        )
    )

    prop "Easing.Linear is monotone" (fun () ->
        Check.One(
            Config.QuickThrowOnFailure.WithMaxTest(100).WithArbitrary([ typeof<Generators> ]),
            fun () ->
                gen {
                    let! t0 = genT
                    let! t1 = genT
                    return t0, t1
                } |> FsGen.sample 100 |> Array.forall (fun (t0, t1) ->
                    if t0 < t1 then Easing.evaluate Linear t0 <= Easing.evaluate Linear t1
                    else true
                )
        )
    )

    prop "Easing clamps input below 0 and above 1" (fun () ->
        Check.One(fsCheckConfig, fun () ->
            [ Linear; EaseIn; EaseOut; EaseInOut ]
            |> List.forall (fun e ->
                Easing.evaluate e -1.0f = Easing.evaluate e 0.0f &&
                Easing.evaluate e  2.0f = Easing.evaluate e 1.0f
            )
        )
    )

// ───────────────────────────────────────────────────────────────────────────────
// M4 — TEA algebraic laws (FsCheck)
// ───────────────────────────────────────────────────────────────────────────────

let testTeaProperties () =
    let counterProgram : FSharp.Graphics.Tea.Program<int, int> = {
        Init   = fun () -> 0, FSharp.Graphics.Tea.Cmd.none
        Update = fun model delta -> model + delta, FSharp.Graphics.Tea.Cmd.none
        View   = fun _ -> Scene.Empty
    }

    prop "TEA: update is deterministic" (fun () ->
        Check.One(fsCheckConfig, fun () ->
            let rt1 = FSharp.Graphics.Runtime.RuntimeHelpers.start counterProgram
            let rt2 = FSharp.Graphics.Runtime.RuntimeHelpers.start counterProgram
            rt1.Dispatch 5
            rt2.Dispatch 5
            rt1.RenderFrame 0.0
            rt2.RenderFrame 0.0
            rt1.Model = rt2.Model
        )
    )

    prop "TEA: init model is reproducible" (fun () ->
        Check.One(fsCheckConfig, fun () ->
            let (m1, _) = counterProgram.Init()
            let (m2, _) = counterProgram.Init()
            m1 = m2
        )
    )

    prop "TEA: runHeadless produces exactly n frames for n batches" (fun () ->
        Check.One(
            fewTestsConfig,
            fun () ->
                gen {
                    let! n = FsGen.choose(1, 6)
                    let! msgs = FsGen.listOfLength n (FsGen.choose(1, 10))
                    return n, msgs |> List.map (fun m -> [m])
                } |> FsGen.sample 20 |> Array.forall (fun (n, batches) ->
                    let frames = FSharp.Graphics.Runtime.RuntimeHelpers.runHeadless counterProgram batches
                    frames.Length = n
                )
        )
    )

    prop "TEA: Cmd.flatten is stable" (fun () ->
        Check.One(fsCheckConfig, fun () ->
            let cmd : FSharp.Graphics.Tea.Cmd<int> =
                FSharp.Graphics.Tea.Batch [
                    FSharp.Graphics.Tea.Emit 1
                    FSharp.Graphics.Tea.Emit 2
                    FSharp.Graphics.Tea.Batch [
                        FSharp.Graphics.Tea.Emit 3
                        FSharp.Graphics.Tea.Batch [ FSharp.Graphics.Tea.Emit 4 ] ] ]
            let once  = FSharp.Graphics.Tea.Cmd.flatten cmd
            let twice = FSharp.Graphics.Tea.Cmd.flatten (FSharp.Graphics.Tea.Batch once)
            once.Length = twice.Length
        )
    )

    prop "TEA: model accumulates correctly across dispatches" (fun () ->
        Check.One(
            fewTestsConfig,
            fun () ->
                gen {
                    let! deltas = FsGen.listOfLength 5 (FsGen.choose(-10, 10))
                    return deltas
                } |> FsGen.sample 50 |> Array.forall (fun deltas ->
                    let rt = FSharp.Graphics.Runtime.RuntimeHelpers.start counterProgram
                    for d in deltas do rt.Dispatch d
                    rt.RenderFrame 0.0
                    rt.Model = List.sum deltas
                )
        )
    )

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// ─────────────────────────────────────────────────────────────────────────────

let runAllPropertyTests () =
    printfn "-- Matrix3x2 properties --"
    testMatrixProperties ()
    printfn ""
    printfn "-- Vec2 properties --"
    testVec2Properties ()
    printfn ""
    printfn "-- Transform consistency --"
    testTransformConsistency ()
    printfn ""
    printfn "-- SDF properties --"
    testSdfProperties ()
    printfn ""
    printfn "-- Hit-test & bounding box properties --"
    testHitTestProperties ()
    printfn ""
    printfn "-- Layout properties --"
    testLayoutProperties ()
    printfn ""
    printfn "-- Diff properties --"
    testDiffProperties ()
    printfn ""
    printfn "-- Color properties --"
    testColorProperties ()
    printfn ""
    printfn "-- Easing properties (M5) --"
    testEasingProperties ()
    printfn ""
    printfn "-- TEA properties (M4) --"
    testTeaProperties ()

    (passed, failed)
