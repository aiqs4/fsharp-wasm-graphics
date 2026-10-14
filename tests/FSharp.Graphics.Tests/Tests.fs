/// Unit tests for FSharp.Graphics – Phase 2-4 verification.
/// Uses a minimal inline test runner (no external framework) so the test
/// project has zero dependencies beyond the library itself.
module FSharp.Graphics.Tests.Tests

open FSharp.Graphics
open FSharp.Graphics.Primitives
open FSharp.Graphics.Primitives.Shapes
open FSharp.Graphics.Scene
open FSharp.Graphics.Layout
open FSharp.Graphics.Render
open FSharp.Graphics.Tea
open FSharp.Graphics.Runtime
open GlyphAuro

// ─────────────────────────────────────────────────────────────────────────────
// Minimal test harness
// ─────────────────────────────────────────────────────────────────────────────

let mutable private passed = 0
let mutable private failed = 0

let private testCase name body =
    try
        body ()
        passed <- passed + 1
        printfn "  PASS  %s" name
    with ex ->
        failed <- failed + 1
        printfn "  FAIL  %s\n         %s" name ex.Message

let private assert' condition msg =
    if not condition then failwith msg

let private assertEq a b label =
    if a <> b then failwithf "%s: expected %A but got %A" label a b

let private assertClose (a: float32) (b: float32) (eps: float32) label =
    if abs (a - b) > eps then
        failwithf "%s: expected %.6f ≈ %.6f (eps %.6f)" label a b eps

// ─────────────────────────────────────────────────────────────────────────────
// 1. Core types
// ─────────────────────────────────────────────────────────────────────────────

let testCoreTypes () =
    testCase "Vec2 arithmetic" (fun () ->
        let a = Vec2.Create(3.0f, 4.0f)
        let b = Vec2.Create(1.0f, 2.0f)
        assertEq (a + b) (Vec2.Create(4.0f, 6.0f)) "add"
        assertEq (a - b) (Vec2.Create(2.0f, 2.0f)) "sub"
        assertClose a.Length 5.0f 1e-5f "length"
    )

    testCase "Rect intersection" (fun () ->
        let a = Rect.Create(0.0f, 0.0f, 10.0f, 10.0f)
        let b = Rect.Create(5.0f, 5.0f, 10.0f, 10.0f)
        let r = Rect.Intersect a b
        assertEq r.X 5.0f "ix"
        assertEq r.Y 5.0f "iy"
        assertEq r.W 5.0f "iw"
        assertEq r.H 5.0f "ih"
    )

    testCase "Rect union" (fun () ->
        let a = Rect.Create(0.0f,  0.0f, 5.0f, 5.0f)
        let b = Rect.Create(3.0f,  3.0f, 5.0f, 5.0f)
        let r = Rect.Union a b
        assertEq r.X 0.0f "ux"
        assertEq r.Y 0.0f "uy"
        assertEq r.W 8.0f "uw"
        assertEq r.H 8.0f "uh"
    )

    testCase "Color.Over compositing" (fun () ->
        let fg = Color.Create(1.0f, 0.0f, 0.0f, 0.5f)   // 50% red
        let bg = Color.Opaque(0.0f, 0.0f, 1.0f)           // solid blue
        let r  = Color.Over fg bg
        assertClose r.A 1.0f 1e-5f "alpha"
        assert' (r.R > 0.3f && r.R < 0.7f) "red component"
        assert' (r.B > 0.3f && r.B < 0.7f) "blue component"
    )

    testCase "Matrix3x2 identity transform" (fun () ->
        let m = Matrix3x2.Identity
        let p = Vec2.Create(3.0f, 7.0f)
        let q = m.Transform p
        assertEq q p "identity transform"
    )

    testCase "Matrix3x2 translation" (fun () ->
        let m = Matrix3x2.Translation(10.0f, 20.0f)
        let p = Vec2.Create(1.0f, 1.0f)
        let q = m.Transform p
        assertClose q.X 11.0f 1e-5f "tx"
        assertClose q.Y 21.0f 1e-5f "ty"
    )

    testCase "Matrix3x2 rotation 90°" (fun () ->
        let angle = System.MathF.PI / 2.0f
        let m = Matrix3x2.Rotation angle
        let p = Vec2.Create(1.0f, 0.0f)
        let q = m.Transform p
        assertClose q.X  0.0f 1e-5f "rx"
        assertClose q.Y  1.0f 1e-5f "ry"
    )

    testCase "Matrix3x2 invert" (fun () ->
        let m = Matrix3x2.Translation(5.0f, -3.0f)
        match m.Invert() with
        | ValueNone -> failwith "expected inverse"
        | ValueSome inv ->
            let comp = m * inv
            assertClose comp.M11 1.0f 1e-5f "m11"
            assertClose comp.M31 0.0f 1e-4f "m31"
    )

// ─────────────────────────────────────────────────────────────────────────────
// 2. SDF shapes
// ─────────────────────────────────────────────────────────────────────────────

let testSdfShapes () =
    testCase "CircleSdf inside is negative" (fun () ->
        let c = CircleSdf(Vec2.Create(0.0f, 0.0f), 10.0f)
        let sdf = (c :> ISdfShape).Sdf(Vec2.Create(0.0f, 0.0f))
        assert' (sdf < 0.0f) (sprintf "expected negative, got %f" sdf)
    )

    testCase "CircleSdf outside is positive" (fun () ->
        let c = CircleSdf(Vec2.Create(0.0f, 0.0f), 10.0f)
        let sdf = (c :> ISdfShape).Sdf(Vec2.Create(20.0f, 0.0f))
        assert' (sdf > 0.0f) (sprintf "expected positive, got %f" sdf)
    )

    testCase "CircleSdf on boundary ≈ 0" (fun () ->
        let c = CircleSdf(Vec2.Create(0.0f, 0.0f), 10.0f)
        let sdf = (c :> ISdfShape).Sdf(Vec2.Create(10.0f, 0.0f))
        assertClose sdf 0.0f 1e-4f "boundary"
    )

    testCase "RectSdf sharp inside" (fun () ->
        let r = RectSdf(Rect.Create(-5.0f, -5.0f, 10.0f, 10.0f))
        let sdf = (r :> ISdfShape).Sdf(Vec2.Zero)
        assert' (sdf < 0.0f) (sprintf "expected negative, got %f" sdf)
    )

    testCase "RectSdf sharp outside" (fun () ->
        let r = RectSdf(Rect.Create(0.0f, 0.0f, 10.0f, 10.0f))
        let sdf = (r :> ISdfShape).Sdf(Vec2.Create(20.0f, 5.0f))
        assert' (sdf > 0.0f) (sprintf "expected positive, got %f" sdf)
    )

    testCase "EllipseSdf bounds" (fun () ->
        let e = EllipseSdf(Vec2.Create(50.0f, 50.0f), 30.0f, 20.0f)
        let bounds = (e :> ISdfShape).Bounds
        assertClose bounds.X 20.0f 1e-4f "bounds.X"
        assertClose bounds.Y 30.0f 1e-4f "bounds.Y"
    )

    testCase "PolygonSdf triangle" (fun () ->
        // Equilateral triangle centred at origin
        let pts = [| Vec2.Create(0.0f, -10.0f)
                     Vec2.Create(8.66f, 5.0f)
                     Vec2.Create(-8.66f, 5.0f) |]
        let p = PolygonSdf pts
        let sdfInside  = (p :> ISdfShape).Sdf(Vec2.Create(0.0f, 0.0f))
        let sdfOutside = (p :> ISdfShape).Sdf(Vec2.Create(0.0f, 20.0f))
        assert' (sdfInside  < 0.0f) (sprintf "inside: %f" sdfInside)
        assert' (sdfOutside > 0.0f) (sprintf "outside: %f" sdfOutside)
    )

    testCase "ShapeCache stores and retrieves" (fun () ->
        let cache = ShapeCache(10)
        let dl : DrawList = [| PopClip |]
        cache.Put("key1", dl)
        match cache.TryGet "key1" with
        | ValueSome _ -> ()
        | ValueNone   -> failwith "expected cached value"
    )

    testCase "ShapeCache evicts on overflow" (fun () ->
        let cache = ShapeCache(2)
        cache.Put("a", [||])
        cache.Put("b", [||])
        cache.Put("c", [||])   // should evict "a"
        assertEq cache.Count 2 "count after eviction"
        match cache.TryGet "a" with
        | ValueSome _ -> failwith "expected evicted"
        | ValueNone   -> ()
    )

// ─────────────────────────────────────────────────────────────────────────────
// 3. Primitives
// ─────────────────────────────────────────────────────────────────────────────

let testPrimitives () =
    testCase "CornerRadius.All" (fun () ->
        let cr = CornerRadius.All 8.0f
        assertEq cr.TL 8.0f "TL"
        assertEq cr.TR 8.0f "TR"
        assertEq cr.BR 8.0f "BR"
        assertEq cr.BL 8.0f "BL"
    )

    testCase "PrimitiveShape.Circle ToSdfShape" (fun () ->
        let p = circleSimple 50.0f 50.0f 30.0f Color.Red
        let sdf = p.ToSdfShape()
        let inside = sdf.Sdf(Vec2.Create(50.0f, 50.0f))
        assert' (inside < 0.0f) "circle centre should be inside"
    )

    testCase "PrimitiveShape.Rectangle bounds" (fun () ->
        let p = rectSimple 10.0f 20.0f 100.0f 80.0f Color.Blue
        let b = p.Bounds
        assertClose b.X 10.0f 1e-4f "x"
        assertClose b.Y 20.0f 1e-4f "y"
        assertClose b.W 100.0f 1e-4f "w"
        assertClose b.H 80.0f  1e-4f "h"
    )

    testCase "Shapes.withStroke" (fun () ->
        let p = circleSimple 0.0f 0.0f 10.0f Color.Green
        let s = Stroke.Solid(Color.Black, 2.0f)
        let p' = withStroke s p
        match p'.Stroke with
        | Some st     -> assertClose st.Thickness 2.0f 1e-5f "thickness"
        | Option.None -> failwith "expected stroke"
    )

// ─────────────────────────────────────────────────────────────────────────────
// 4. Text node
// ─────────────────────────────────────────────────────────────────────────────

let testTextNode () =
    let makeNode text =
        { Text          = text
          Font          = FontFace.SansSerif 16.0f
          Color         = Color.Black
          Align         = TextAlign.Left
          MaxWidth      = ValueNone
          MaxHeight     = ValueNone
          LetterSpacing = 0.0f
          LineSpacing   = 1.0f }

    testCase "TextNode.layout produces glyphs" (fun () ->
        let node = makeNode "Hello"
        let glyphs, bounds = TextNode.layout node
        assert' (glyphs.Length = 5) (sprintf "expected 5 glyphs, got %d" glyphs.Length)
        assert' (bounds.W > 0.0f) "width > 0"
        assert' (bounds.H > 0.0f) "height > 0"
    )

    testCase "TextNode.layout empty string" (fun () ->
        let node = makeNode ""
        let glyphs, bounds = TextNode.layout node
        assertEq glyphs.Length 0 "no glyphs"
        assertEq bounds Rect.Zero "zero bounds"
    )

    testCase "TextNode.layout wraps at MaxWidth" (fun () ->
        let node = { makeNode "Hello World" with MaxWidth = ValueSome 40.0f }
        let glyphs, bounds = TextNode.layout node
        assert' (glyphs.Length > 0) "has glyphs"
        // Two lines expected: "Hello" + "World"
        let lineH = node.Font.LineHeight
        assert' (bounds.H >= lineH * 2.0f - 1.0f)
                (sprintf "expected ≥ 2 lines, H=%.1f lineH=%.1f" bounds.H lineH)
    )

    testCase "TextNode.hitTest returns valid index" (fun () ->
        let node = makeNode "Hello"
        let idx = TextNode.hitTest node (Vec2.Create(0.0f, 0.0f))
        assert' (idx >= 0 && idx <= 5) (sprintf "index %d out of range" idx)
    )

    testCase "TextNode.caretPosition for index 0" (fun () ->
        let node = makeNode "Hello"
        let r = TextNode.caretPosition node 0
        assert' (r.W > 0.0f) "caret width"
        assert' (r.H > 0.0f) "caret height"
    )

    testCase "TextNode.selectionRects" (fun () ->
        let node = makeNode "Hello"
        let rects = TextNode.selectionRects node 0 3
        assertEq rects.Length 3 "3 chars selected"
    )

    testCase "TextNode.update uses cache" (fun () ->
        let node  = makeNode "Cached"
        let cache = ShapeCache(32)
        let dl1   = TextNode.update node cache
        let dl2   = TextNode.update node cache
        assert' (obj.ReferenceEquals(dl1, dl2)) "same array reference on second call"
    )

// ─────────────────────────────────────────────────────────────────────────────
// 5. Layout engine
// ─────────────────────────────────────────────────────────────────────────────

let testLayout () =
    let avail = Vec2.Create(400.0f, 300.0f)

    testCase "measure Empty = zero" (fun () ->
        let s = LayoutEngine.measure Empty avail
        assertEq s Vec2.Zero "empty"
    )

    testCase "measure Shape" (fun () ->
        let scene = rectSimple 0.0f 0.0f 100.0f 50.0f Color.Red |> Shape
        let s = LayoutEngine.measure scene avail
        assertClose s.X 100.0f 1e-4f "w"
        assertClose s.Y 50.0f  1e-4f "h"
    )

    testCase "measure Padding adds insets" (fun () ->
        let inner = rectSimple 0.0f 0.0f 100.0f 50.0f Color.Red |> Shape
        let scene = Padding(Insets.All 10.0f, inner)
        let s = LayoutEngine.measure scene avail
        assertClose s.X 120.0f 1e-4f "w with padding"
        assertClose s.Y 70.0f  1e-4f "h with padding"
    )

    testCase "measure VStack sums heights" (fun () ->
        let child = rectSimple 0.0f 0.0f 100.0f 50.0f Color.Blue |> Shape
        let scene = Stack(Vertical, 10.0f, Alignment.Start, [ child; child ])
        let s = LayoutEngine.measure scene avail
        assertClose s.Y 110.0f 1e-4f "h = 50+10+50"
    )

    testCase "measure HStack sums widths" (fun () ->
        let child = rectSimple 0.0f 0.0f 80.0f 40.0f Color.Green |> Shape
        let scene = Stack(Horizontal, 5.0f, Alignment.Start, [ child; child ])
        let s = LayoutEngine.measure scene avail
        assertClose s.X 165.0f 1e-4f "w = 80+5+80"
    )

    testCase "layout positions VStack children" (fun () ->
        let child = rectSimple 0.0f 0.0f 50.0f 30.0f Color.Red |> Shape
        let scene = Stack(Vertical, 0.0f, Alignment.Start, [ child; child ])
        let result = LayoutEngine.layout scene Vec2.Zero avail
        assertClose result.Bounds.H 60.0f 1e-4f "total height"
    )

    testCase "layoutScene viewport" (fun () ->
        let scene = rectSimple 0.0f 0.0f 100.0f 100.0f Color.White |> Shape
        let vp = Rect.Create(0.0f, 0.0f, 400.0f, 300.0f)
        let r = LayoutEngine.layoutScene scene vp
        assert' (not r.Bounds.IsEmpty) "non-empty bounds"
    )

// ─────────────────────────────────────────────────────────────────────────────
// 6. Diff
// ─────────────────────────────────────────────────────────────────────────────

let testDiff () =
    testCase "diff None prev = Replace" (fun () ->
        let scene = rectSimple 0.0f 0.0f 10.0f 10.0f Color.Red |> Shape
        let delta = Diff.diffOpt Option.None scene
        match delta with
        | Replace _ -> ()
        | _         -> failwithf "expected Replace, got %A" delta
    )

    testCase "diff identical scene = NoChange" (fun () ->
        let scene = rectSimple 0.0f 0.0f 10.0f 10.0f Color.Red |> Shape
        let delta = Diff.diffOpt (Some scene) scene
        match delta with
        | NoChange -> ()
        | _        -> failwithf "expected NoChange, got %A" delta
    )

    testCase "diff changed opacity = UpdateOpacity" (fun () ->
        let child = rectSimple 0.0f 0.0f 10.0f 10.0f Color.Blue |> Shape
        let prev  = Opacity(1.0f, child)
        let next  = Opacity(0.5f, child)
        let delta = Diff.diff prev next
        match delta with
        | UpdateOpacity 0.5f -> ()
        | _                  -> failwithf "expected UpdateOpacity 0.5, got %A" delta
    )

    testCase "diff changed transform = UpdateTransform" (fun () ->
        let child = Empty
        let prev  = Transform(Matrix3x2.Identity, child)
        let next  = Transform(Matrix3x2.Translation(5.0f, 5.0f), child)
        let delta = Diff.diff prev next
        match delta with
        | UpdateTransform _ -> ()
        | _                 -> failwithf "expected UpdateTransform, got %A" delta
    )

    testCase "diff group child change = UpdateChildren" (fun () ->
        let c1 = rectSimple 0.0f 0.0f 10.0f 10.0f Color.Red   |> Shape
        let c2 = rectSimple 0.0f 0.0f 10.0f 10.0f Color.Blue  |> Shape
        let c3 = rectSimple 0.0f 0.0f 10.0f 10.0f Color.Green |> Shape
        let prev = Group [ c1; c2 ]
        let next = Group [ c1; c3 ]
        let delta = Diff.diff prev next
        match delta with
        | UpdateChildren [(1, Replace _)] -> ()
        | _                               -> failwithf "unexpected delta %A" delta
    )

// ─────────────────────────────────────────────────────────────────────────────
// 7. Encoder
// ─────────────────────────────────────────────────────────────────────────────

let testEncoder () =
    testCase "encode empty DrawList has BeginFrame + EndFrame" (fun () ->
        let (buf, len) = Encode.encode [||]
        assert' (len >= 2) "at least 2 bytes"
        assertEq buf.[0] Encode.OpcodeBeginFrame "begin opcode"
        assertEq buf.[len - 1] Encode.OpcodeEndFrame "end opcode"
    )

    testCase "encode SetOpacity" (fun () ->
        let (buf, len) = Encode.encode [| SetOpacity 0.5f |]
        assert' (len > 6) "has content"
        // After BeginFrame, next opcode is SetOpacity
        assertEq buf.[1] Encode.OpcodeSetOpacity "setopacity opcode"
    )

    testCase "encode PushClip + PopClip" (fun () ->
        let clip = Rect.Create(0.0f, 0.0f, 100.0f, 100.0f)
        let (buf, _) = Encode.encode [| PushClip clip; PopClip |]
        assertEq buf.[1] Encode.OpcodePushClip "pushclip"
    )

    testCase "encodeInto respects buffer" (fun () ->
        let buf = Array.zeroCreate 4096
        let len = Encode.encodeInto [| SetTransform Matrix3x2.Identity |] buf
        assert' (len > 0) "bytes written"
        assertEq buf.[0] Encode.OpcodeBeginFrame "begin"
    )

    testCase "encode DrawTextBatch" (fun () ->
        let glyphs : GlyphPosition array =
            [| { GlyphId = 65u; X = 0.0f; Y = 0.0f; AdvanceX = 10.0f } |]
        let (buf, len) = Encode.encode [| DrawTextBatch(glyphs, 1UL, 16.0f, Color.Black) |]
        assert' (len > 20) "DrawTextBatch has content"
        assertEq buf.[1] Encode.OpcodeDrawText "text opcode"
    )

// ─────────────────────────────────────────────────────────────────────────────
// 8. Render internal cache
// ─────────────────────────────────────────────────────────────────────────────

let testRenderInternal () =
    testCase "CachedNode.build Shape" (fun () ->
        let scene = circleSimple 50.0f 50.0f 20.0f Color.Red |> Shape
        let node  = Render.Internal.CachedNode.build scene
        match node with
        | Render.Internal.CachedShape _ -> ()
        | _ -> failwith "expected CachedShape"
    )

    testCase "CachedNode.build Group" (fun () ->
        let scene = Group [ Empty; Empty ]
        let node  = Render.Internal.CachedNode.build scene
        match node with
        | Render.Internal.CachedGroup(children, _, _) ->
            assertEq children.Length 2 "two children"
        | _ -> failwith "expected CachedGroup"
    )

    testCase "CachedNode.getDrawList Group" (fun () ->
        let c1 = circleSimple 0.0f 0.0f 10.0f Color.Red   |> Shape
        let c2 = rectSimple   0.0f 0.0f 10.0f 10.0f Color.Blue |> Shape
        let scene = Group [ c1; c2 ]
        let node  = Render.Internal.CachedNode.build scene
        let cache = ShapeCache(32)
        let dl    = Render.Internal.CachedNode.getDrawList node cache
        assert' (dl.Length > 0) "non-empty draw list"
    )

    testCase "applyDelta NoChange returns same" (fun () ->
        let scene = Empty
        let node  = Render.Internal.CachedNode.build scene
        let node' = Render.Internal.CachedNode.applyDelta NoChange node
        assert' (obj.ReferenceEquals(node, node')) "same reference"
    )

    testCase "applyDelta Replace rebuilds" (fun () ->
        let prev  = Render.Internal.CachedNode.build Empty
        let next  = circleSimple 0.0f 0.0f 10.0f Color.Red |> Shape
        let node' = Render.Internal.CachedNode.applyDelta (Replace next) prev
        match node' with
        | Render.Internal.CachedShape _ -> ()
        | _ -> failwith "expected CachedShape after Replace"
    )

// ─────────────────────────────────────────────────────────────────────────────
// 9. TEA
// ─────────────────────────────────────────────────────────────────────────────

let testTea () =
    let counterProgram : Program<int, bool> =
        { Init   = fun () -> 0, Cmd.none
          Update = fun model inc ->
                       model + (if inc then 1 else -1), Cmd.none
          View   = fun count ->
                       Text {
                           Text  = string count
                           Font  = FontFace.SansSerif 14.0f
                           Color = Color.Black
                           Align = TextAlign.Left
                           MaxWidth      = ValueNone
                           MaxHeight     = ValueNone
                           LetterSpacing = 0.0f
                           LineSpacing   = 1.0f } }

    testCase "Runtime.runHeadless produces one frame per input batch" (fun () ->
        let frames = RuntimeHelpers.runHeadless counterProgram
                         [ [true; true]; [false] ]
        assertEq frames.Length 2 "two frames"
    )

    testCase "Runtime.runHeadless frames are non-empty" (fun () ->
        let frames = RuntimeHelpers.runHeadless counterProgram [ [true] ]
        assert' (frames.Head.Length > 0) "frame has bytes"
    )

    testCase "Cmd.flatten Batch" (fun () ->
        let cmd : Cmd<int> = Batch [ Emit 1; Emit 2; Batch [ Emit 3 ] ]
        let flat = Cmd.flatten cmd
        assertEq flat.Length 3 "three leaf commands"
    )

    testCase "Runtime model update" (fun () ->
        let rt = RuntimeHelpers.start counterProgram
        rt.Dispatch true
        rt.Dispatch true
        rt.RenderFrame 0.0
        assertEq rt.Model 2 "model after two increments"
    )

// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────

[<EntryPoint>]
let main _ =
    printfn "=== FSharp.Graphics Tests ==="
    printfn ""
    printfn "-- Core types --"
    testCoreTypes ()
    printfn ""
    printfn "-- SDF shapes --"
    testSdfShapes ()
    printfn ""
    printfn "-- Primitives --"
    testPrimitives ()
    printfn ""
    printfn "-- Text node --"
    testTextNode ()
    printfn ""
    printfn "-- Layout --"
    testLayout ()
    printfn ""
    printfn "-- Diff --"
    testDiff ()
    printfn ""
    printfn "-- Encoder --"
    testEncoder ()
    printfn ""
    printfn "-- Render internal --"
    testRenderInternal ()
    printfn ""
    printfn "-- TEA --"
    testTea ()
    printfn ""
    printfn "==========================="
    printfn "Results: %d passed, %d failed" passed failed
    if failed > 0 then 1 else 0
