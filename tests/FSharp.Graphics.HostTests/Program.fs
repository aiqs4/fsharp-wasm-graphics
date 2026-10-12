module FSharp.Graphics.HostTests.Program

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.Render
open FSharp.Graphics.Host
open FSharp.Graphics.Scenes
open FSharp.Graphics.Scenes.Kit

// ── mini harness ───────────────────────────────────────────────────────────
let mutable passed = 0
let failures = ResizeArray<string>()
let check (name: string) (ok: bool) =
    if ok then passed <- passed + 1 else failures.Add name; printfn "  FAIL  %s" name
let near (name: string) (eps: float32) (expected: float32) (actual: float32) =
    check (sprintf "%s (expected %.3f, got %.3f)" name expected actual) (MathF.Abs(expected - actual) <= eps)
let section (s: string) = printfn "%s" s

let v = Vec2.create

// ── 1. SDFs (the blur / half-transparent-fill bug) ─────────────────────────
let sdfTests () =
    section "SDF correctness"
    let rect = RectSdf(Rect.create 10.0f 10.0f 100.0f 60.0f, CornerRadius.all 12.0f) :> ISdfShape
    near "rect: straight edge midpoint is 0" 1e-3f 0.0f (rect.Sdf(v 110.0f 40.0f))
    near "rect: 5px inside a straight edge is -5 (old formula gave the wrong value)" 1e-3f -5.0f (rect.Sdf(v 105.0f 40.0f))
    near "rect: 10px outside a straight edge is +10" 1e-3f 10.0f (rect.Sdf(v 120.0f 40.0f))
    near "rect: centre is -min(hw,hh)" 1e-3f -30.0f (rect.Sdf(v 60.0f 40.0f))
    near "rect: corner distance follows the arc" 0.01f (MathF.Sqrt 288.0f - 12.0f) (rect.Sdf(v 110.0f 10.0f))
    let sharp = RectSdf(Rect.create 0.0f 0.0f 50.0f 50.0f, CornerRadius.zero) :> ISdfShape
    near "sharp rect: inside is negative, not 0 (old code gave half-alpha fills)" 1e-3f -10.0f (sharp.Sdf(v 10.0f 25.0f))

    let circle = CircleSdf(v 0.0f 0.0f, 20.0f) :> ISdfShape
    near "circle boundary" 1e-4f 0.0f (circle.Sdf(v 20.0f 0.0f))
    near "circle centre" 1e-4f -20.0f (circle.Sdf(v 0.0f 0.0f))

    let ell = EllipseSdf(v 0.0f 0.0f, 60.0f, 30.0f) :> ISdfShape
    near "ellipse boundary (x axis)" 1e-3f 0.0f (ell.Sdf(v 60.0f 0.0f))
    near "ellipse boundary (y axis)" 1e-3f 0.0f (ell.Sdf(v 0.0f 30.0f))
    near "ellipse 5px outside the x vertex" 0.5f 5.0f (ell.Sdf(v 65.0f 0.0f))
    near "ellipse 5px outside the y vertex" 0.5f 5.0f (ell.Sdf(v 0.0f 35.0f))

    // |gradient| ~ 1 near the boundary => anti-aliasing / stroke widths are in true pixels
    let gradOk (name: string) (s: ISdfShape) (lo: float32) (hi: float32) =
        let b = s.Bounds
        let mutable worst = 1.0f
        for i in 0 .. 60 do
            for j in 0 .. 60 do
                let p = v (b.X - 6.0f + float32 i * (b.W + 12.0f) / 60.0f) (b.Y - 6.0f + float32 j * (b.H + 12.0f) / 60.0f)
                if MathF.Abs(s.Sdf p) < 2.0f then
                    let e = 0.01f
                    let gx = (s.Sdf(v (p.X + e) p.Y) - s.Sdf(v (p.X - e) p.Y)) / (2.0f * e)
                    let gy = (s.Sdf(v p.X (p.Y + e)) - s.Sdf(v p.X (p.Y - e))) / (2.0f * e)
                    let g = MathF.Sqrt(gx * gx + gy * gy)
                    if g > 0.05f && MathF.Abs(g - 1.0f) > MathF.Abs(worst - 1.0f) then worst <- g
        check (sprintf "%s: |grad| in [%.2f, %.2f] near the edge (worst %.3f)" name lo hi worst) (worst >= lo && worst <= hi)
    gradOk "rect" rect 0.97f 1.03f
    gradOk "circle" circle 0.97f 1.03f
    gradOk "ellipse" ell 0.85f 1.2f
    gradOk "polygon" (PolygonSdf [| v 0.0f 0.0f; v 80.0f 10.0f; v 50.0f 70.0f |] :> ISdfShape) 0.97f 1.03f

// ── 2. protocol ────────────────────────────────────────────────────────────
type Recorder() =
    member val Log = ResizeArray<string>() with get
    member val Depth = 0 with get, set
    member val MinDepth = 0 with get, set
    interface IRenderSink with
        member r.BeginFrame(ver, seq) = r.Log.Add(sprintf "begin v%d #%d" ver seq)
        member r.EndFrame() = r.Log.Add "end"
        member r.Fill(g, _) = r.Log.Add(sprintf "fill %s" (g.GetType().Name))
        member r.Stroke(g, s) = r.Log.Add(sprintf "stroke %s w=%.1f" (g.GetType().Name) s.Width)
        member r.Shadow(g, s) = r.Log.Add(sprintf "shadow %s inner=%b" (g.GetType().Name) s.Inner)
        member r.Text(_, sz, _, gs) = r.Log.Add(sprintf "text %.0f x%d" sz gs.Length)
        member r.Image(_, _, _) = r.Log.Add "image"
        member r.PushClip(_) = r.Depth <- r.Depth + 1; r.Log.Add "pushclip"
        member r.PopClip() = r.Depth <- r.Depth - 1; r.MinDepth <- min r.MinDepth r.Depth; r.Log.Add "popclip"
        member r.PushTransform(_) = r.Depth <- r.Depth + 1; r.Log.Add "pushxf"
        member r.PopTransform() = r.Depth <- r.Depth - 1; r.MinDepth <- min r.MinDepth r.Depth; r.Log.Add "popxf"
        member r.PushLayer(_) = r.Depth <- r.Depth + 1; r.Log.Add "pushlayer"
        member r.PopLayer() = r.Depth <- r.Depth - 1; r.MinDepth <- min r.MinDepth r.Depth; r.Log.Add "poplayer"

let protocolTests (text: ITextEngine) =
    section "Command protocol"
    let cmds : DrawList =
        [ DrawRect(Rect.create 0.0f 0.0f 10.0f 10.0f, CornerRadius.all 2.0f, Solid Color.White)
          DrawCircle(v 5.0f 5.0f, 3.0f, Paint.radial (v 5.0f 5.0f) 3.0f [| { Offset = 0.0f; Color = Color.White }; { Offset = 1.0f; Color = Color.Black } |])
          DrawStroke(RectSdf(Rect.create 0.0f 0.0f 10.0f 10.0f, CornerRadius.zero), Stroke.solid Color.White 2.0f)
          DrawShadow(CircleSdf(v 5.0f 5.0f, 3.0f), Shadow.inner (v 0.0f 1.0f) 4.0f Color.Black)
          PushTransform Mat3x2.identity; PushClip(Rect.create 0.0f 0.0f 5.0f 5.0f); PopClip; PopTransform
          PushLayer { LayerParams.none with Alpha = 0.5f; BlurSigma = 2.0f; Filter = Array.zeroCreate 20 }; PopLayer ]
    let (buf, len) = Encode.encode cmds
    check "stream starts with BeginFrame, version 2" (buf.[0] = Protocol.OpBeginFrame && buf.[1] = Protocol.Version)
    check "BeginFrame is 6 bytes (the old runner read it as 5 and desynced -> 'unknown opcode 0x0')" (buf.[6] = Protocol.OpFill)
    check "stream ends with EndFrame" (buf.[len - 1] = Protocol.OpEndFrame)
    let rec' = Recorder()
    match CommandReader.run rec' buf len with
    | Decoded n ->
        check "round trip: every command decoded" (n = 10)
        check "round trip: stroke keeps its shape (old encoder dropped it)" (rec'.Log.Contains "stroke GRect w=2.0")
        check "round trip: shadow keeps its shape + inner flag" (rec'.Log.Contains "shadow GCircle inner=true")
        check "push/pop balanced" (rec'.Depth = 0 && rec'.MinDepth = 0)
    | Malformed(o, m) -> check (sprintf "round trip decodes (malformed @%d: %s)" o m) false

    let bad = Array.copy buf
    bad.[7] <- 0x77uy
    check "corrupt geometry tag -> Malformed, no exception" (match CommandReader.run (Recorder()) bad len with Malformed _ -> true | _ -> false)
    check "truncated stream -> Malformed, no exception" (match CommandReader.run (Recorder()) buf (len - 5) with Malformed _ -> true | _ -> false)
    let zeros = Array.zeroCreate<byte> 64
    check "zero padding is rejected cleanly (the old '0x0' flood)" (match CommandReader.run (Recorder()) zeros 64 with Malformed _ -> true | _ -> false)

    // every demo scene must produce a clean, balanced stream
    for d in Scenes.all do
        let stage = Stage(text, 800.0f, 600.0f)
        let ctx = { Time = 700.0f; Size = v 800.0f 600.0f; Font = FontId 0UL }
        let struct (b, l) = stage.Frame(d.Build ctx)
        let r = Recorder()
        let ok = match CommandReader.run r b l with Decoded _ -> true | _ -> false
        check (sprintf "scene '%s': stream decodes and push/pop balance" d.Id) (ok && r.Depth = 0 && r.MinDepth = 0)

// ── 3. layout / resolve ────────────────────────────────────────────────────
let rec leaves (m: Mat3x2) (s: Scene) : Rect list =
    match s with
    | Shape p -> [ Mat3x2.transformRect m (Shapes.bounds p) ]
    | Transform(t, c) -> leaves (Mat3x2.multiply t m) c
    | Group cs | Stack(_, cs) -> cs |> List.collect (leaves m)
    | Padding(_, c) | Opacity(_, c) | Reactive(_, c) | Blur(_, c) | Scene.Clip(_, c) -> leaves m c
    | _ -> []

let layoutTests () =
    section "Layout (Resolve)"
    let te = TextEngine.approximate
    let bar w h = box 0.0f 0.0f w h red
    let resolved s = Resolve.resolve te 800.0f s
    let ys = Scene.vstack 8.0f [ bar 100.0f 24.0f; bar 100.0f 24.0f; bar 100.0f 24.0f ] |> resolved |> leaves Mat3x2.identity |> List.map (fun r -> r.Y)
    check "vstack places bars at y = 0, 32, 64 (they used to pile up at the origin)" (ys = [ 0.0f; 32.0f; 64.0f ])
    let xs = Scene.hstack 10.0f [ bar 40.0f 10.0f; bar 30.0f 10.0f ] |> resolved |> leaves Mat3x2.identity |> List.map (fun r -> r.X)
    check "hstack places children at x = 0, 50" (xs = [ 0.0f; 50.0f ])
    let centred = hstackA 0.0f CrossAxisAlignment.Center [ bar 10.0f 40.0f; bar 10.0f 10.0f ] |> resolved |> leaves Mat3x2.identity
    near "cross-axis Center: small child sits mid-height" 1e-4f 15.0f (centred.[1].Y)
    let ended = hstackA 0.0f CrossAxisAlignment.End_ [ bar 10.0f 40.0f; bar 10.0f 10.0f ] |> resolved |> leaves Mat3x2.identity
    near "cross-axis End: small child sits at the bottom" 1e-4f 30.0f (ended.[1].Y)
    let padded = Scene.padAll 12.0f (bar 20.0f 20.0f) |> resolved |> leaves Mat3x2.identity
    check "padAll 12 moves the child to (12, 12)" (padded.Head.X = 12.0f && padded.Head.Y = 12.0f)
    let nested = Scene.hstack 0.0f [ Scene.padAll 12.0f (bar 20.0f 20.0f); bar 10.0f 10.0f ] |> resolved |> leaves Mat3x2.identity
    near "padding contributes to the parent's size" 1e-4f 44.0f nested.[1].X

// ── 4. hit testing / dispatch ──────────────────────────────────────────────
let hitTests () =
    section "Hit testing and dispatch"
    let hits = ResizeArray<string>()
    let tag name (e: InputEvent) =
        (match e with
         | PointerDown _ -> hits.Add(name + ":down") | PointerEnter _ -> hits.Add(name + ":enter")
         | PointerLeave _ -> hits.Add(name + ":leave") | PointerMove _ -> hits.Add(name + ":move")
         | PointerUp _ -> hits.Add(name + ":up") | _ -> ())
        true
    let build () =
        Group [ Scene.Reactive(tag "back", box 0.0f 0.0f 100.0f 100.0f blue)
                Scene.Reactive(tag "front", box 20.0f 20.0f 40.0f 40.0f red)
                Scene.translate 200.0f 0.0f (Scene.rotate 90.0f Vec2.zero (Scene.Reactive(tag "rot", box 0.0f 0.0f 50.0f 10.0f green))) ]
    let scene = build ()
    check "topmost wins" ((Hit.pick scene (v 30.0f 30.0f)) |> Array.length = 1)
    check "outside everything -> nothing" ((Hit.pick scene (v 150.0f 150.0f)).Length = 0)
    check "rotated + translated node is hit at its transformed position" ((Hit.pick scene (v 195.0f 20.0f)).Length = 1)
    check "...and not at its untransformed position" ((Hit.pick scene (v 220.0f 5.0f)).Length = 0)

    let d = Hit.Dispatcher()
    d.Handle(build (), PointerMove(v 30.0f 30.0f)) |> ignore
    d.Handle(build (), PointerMove(v 31.0f 31.0f)) |> ignore      // scene rebuilt (new closures): must NOT re-enter
    check "enter fires exactly once across rebuilt scenes (identity = tree path)" (hits |> Seq.filter ((=) "front:enter") |> Seq.length = 1)
    d.Handle(build (), PointerMove(v 90.0f 90.0f)) |> ignore
    check "leave fires when the pointer moves to another node" (hits.Contains "front:leave" && hits.Contains "back:enter")
    hits.Clear()
    d.Handle(build (), PointerDown(v 30.0f 30.0f, 0)) |> ignore
    d.Handle(build (), PointerMove(v 500.0f 500.0f)) |> ignore
    d.Handle(build (), PointerUp(v 500.0f 500.0f, 0)) |> ignore
    check "press captures the pointer: move/up still reach the pressed node" (hits.Contains "front:move" && hits.Contains "front:up")

// ── 5. timeline ────────────────────────────────────────────────────────────
let timelineTests () =
    section "Timeline / easing"
    let lin = Transition.linear 1000
    near "linear @0" 1e-4f 0.0f (Timeline.progress lin 0.0f)
    near "linear @250" 1e-4f 0.25f (Timeline.progress lin 250.0f)
    near "linear clamps after the end" 1e-4f 1.0f (Timeline.progress lin 5000.0f)
    near "delay holds at 0" 1e-4f 0.0f (Timeline.progress { lin with DelayMs = 500.0f } 400.0f)
    near "alternate returns" 1e-3f 0.5f (Timeline.progress { lin with Loop = Alternate } 1500.0f)
    near "infinite wraps" 1e-3f 0.25f (Timeline.progress { lin with Loop = Infinite } 3250.0f)
    check "finite loop reports done" (Timeline.isDone { lin with Loop = Repeat 2 } 2000.0f)
    near "easeInOut midpoint" 1e-3f 0.5f (Easing.evaluate EaseInOut 0.5f)
    near "CSS 'ease' cubic-bezier(.25,.1,.25,1) @0.5 = 0.8024" 5e-3f 0.8024f (Easing.evaluate (CubicBezier(v 0.25f 0.1f, v 0.25f 1.0f)) 0.5f)
    near "cubic-bezier hits 1 at 1" 1e-3f 1.0f (Easing.evaluate (CubicBezier(v 0.25f 0.1f, v 0.25f 1.0f)) 1.0f)

// ── 6. pixels (software backend) ───────────────────────────────────────────
let render (fonts: FontStore) (text: ITextEngine) (w: int) (h: int) (scene: Scene) =
    let stage = Stage(text, float32 w, float32 h)
    let struct (b, l) = stage.Frame scene
    let r = SoftwareRenderer(w, h, fonts)
    match CommandReader.run r b l with
    | Decoded _ -> ()
    | Malformed(o, m) -> failwithf "malformed @%d: %s" o m
    let px = r.ToRgba8 Color.Transparent
    fun (x: int) (y: int) -> let i = (y * w + x) * 4 in (int px.[i], int px.[i + 1], int px.[i + 2], int px.[i + 3])

let pixelTests (fonts: FontStore) (text: ITextEngine) =
    section "Software backend pixels"
    let r = render fonts text 100 100
    let solid = r (Group [ box 10.0f 10.0f 20.0f 20.0f (Color.rgba 200uy 100uy 50uy 255uy) ])
    check "rect interior is the exact colour (not half-transparent)" (solid 20 20 = (200, 100, 50, 255))
    check "outside the rect is untouched" (solid 5 5 = (0, 0, 0, 0))
    let (_, _, _, aEdge) = solid 10 10
    check "pixel-aligned rect edge is fully covered" (aEdge = 255)

    let half = r (box 10.5f 10.0f 10.0f 10.0f Color.White)
    let (_, _, _, a0) = half 10 15
    check (sprintf "half-pixel edge gets ~50%% coverage (got %d)" a0) (abs (a0 - 128) <= 3)

    let moved = r (Scene.translate 40.0f 30.0f (box 0.0f 0.0f 10.0f 10.0f Color.White))
    check "translate moves the shape (the old rasterizer ignored it)" (moved 45 35 = (255, 255, 255, 255) && (let (_, _, _, a) = moved 5 5 in a = 0))

    let rot = r (Scene.rotate 45.0f (v 50.0f 50.0f) (box 40.0f 40.0f 20.0f 20.0f Color.White))
    check "45° rotation: diamond tip reaches beyond the unrotated box" (let (_, _, _, a) = rot 50 38 in a > 200)
    check "...and the unrotated corner is now empty" (let (_, _, _, a) = rot 41 41 in a = 0 || a < 255)

    let rr = r (rbox 10.0f 10.0f 40.0f 40.0f 16.0f Color.White)
    check "rounded corner pixel is transparent, edge midpoint is solid" ((let (_, _, _, a) = rr 10 10 in a < 40) && (let (_, _, _, a) = rr 30 10 in a = 255))

    let st = r (Shape (Shapes.withStroke (Stroke.solid red 4.0f) (Shapes.rect (Rect.create 20.0f 20.0f 40.0f 40.0f) (Solid Color.Transparent))))
    check "stroke is centred on the edge and present along straight edges (old SDF: corners only)" ((let (_, _, _, a) = st 40 20 in a = 255) && (let (_, _, _, a) = st 40 40 in a = 0))

    let g = r (Shape (Shapes.rect (Rect.create 0.0f 0.0f 100.0f 10.0f) (Paint.linearH (Rect.create 0.0f 0.0f 100.0f 10.0f) [| { Offset = 0.0f; Color = Color.Black }; { Offset = 1.0f; Color = Color.White } |])))
    let (r1, _, _, _) = g 10 5
    let (r2, _, _, _) = g 90 5
    check (sprintf "linear gradient spans the box (%d -> %d)" r1 r2) (r1 < 40 && r2 > 215)

    let both = Group [ dot 40.0f 50.0f 20.0f blue; dot 60.0f 50.0f 20.0f blue ]
    let opaque = r (Scene.opacity 0.5f both)
    let (_, _, _, aSingle) = opaque 25 50
    let (_, _, _, aOverlap) = opaque 50 50
    check (sprintf "group opacity is flat where shapes overlap (%d vs %d)" aSingle aOverlap) (abs (aSingle - aOverlap) <= 1 && abs (aSingle - 128) <= 2)
    let perShape = r (Group [ dot 40.0f 50.0f 20.0f (alpha 0.5f blue); dot 60.0f 50.0f 20.0f (alpha 0.5f blue) ])
    check "...unlike per-shape alpha, which darkens the overlap" (let (_, _, _, a) = perShape 50 50 in a > 180)

    let mul = r (Group [ box 0.0f 0.0f 100.0f 100.0f (Color.rgba 128uy 128uy 128uy 255uy)
                         Scene.blendMode Multiply (box 0.0f 0.0f 100.0f 100.0f (Color.rgba 128uy 128uy 128uy 255uy)) ])
    check "multiply blend: 0.5 * 0.5 = 0.25" (let (c, _, _, _) = mul 50 50 in abs (c - 64) <= 2)

    let sh = r (Shape (Shapes.rect (Rect.create 30.0f 30.0f 40.0f 40.0f) (Solid Color.White) |> Shapes.withShadow (Shadow.drop (v 0.0f 0.0f) 16.0f Color.Black)))
    let a1 = (let (_, _, _, a) = sh 72 50 in a)
    let a2 = (let (_, _, _, a) = sh 80 50 in a)
    let a3 = (let (_, _, _, a) = sh 92 50 in a)
    check (sprintf "drop shadow fades smoothly outside the shape (%d > %d > %d)" a1 a2 a3) (a1 > a2 && a2 > a3 && a1 > 0)
    let inner = r (Shape (Shapes.rect (Rect.create 10.0f 10.0f 80.0f 80.0f) (Solid Color.White) |> Shapes.withShadow (Shadow.inner (v 0.0f 6.0f) 8.0f Color.Black)))
    check "inner shadow darkens the top inside edge and leaves the centre alone" ((let (c, _, _, _) = inner 50 12 in c < 160) && (let (c, _, _, _) = inner 50 55 in c > 240))

    // text
    let t = r (Group [ Scene.translate 10.0f 10.0f (Scene.Text("IIII", { TextStyle.defaultStyle (FontId 0UL) with SizePx = 40.0f; Color = Color.White; Wrap = false })) ])
    let mutable lit = 0
    for y in 0 .. 99 do for x in 0 .. 99 do (let (_, _, _, a) = t x y in if a > 128 then lit <- lit + 1)
    check (sprintf "text draws real glyph coverage (%d lit pixels)" lit) (lit > 200 && lit < 3000)

[<EntryPoint>]
let main _ =
    let fonts = try Some (FontLocator.loadDefault ()) with _ -> None
    sdfTests ()
    layoutTests ()
    hitTests ()
    timelineTests ()
    match fonts with
    | Some f ->
        let text = GlyphAuraTextEngine f :> ITextEngine
        protocolTests text
        pixelTests f text
    | None ->
        printfn "SKIP  protocol + pixel tests: no TrueType font found (set FSG_FONT=/path/DejaVuSans.ttf)"
        protocolTests TextEngine.approximate
    printfn "\n%d passed, %d failed" passed failures.Count
    if failures.Count = 0 then 0 else 1
