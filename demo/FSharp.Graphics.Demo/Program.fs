/// Demo entry-point: builds a static scene with several shapes and text, then
/// rasterizes it to a PNG file using the pure-F# software renderer.
///
/// The same scene description also exercises the layout engine, diff
/// algorithm, and command-buffer encoder so the entire Phase 2–4 pipeline
/// is exercised in a single run.
module FSharp.Graphics.Demo.Program

open FSharp.Graphics
open FSharp.Graphics.Primitives
open FSharp.Graphics.Primitives.Shapes
open FSharp.Graphics.Scene
open FSharp.Graphics.Layout
open FSharp.Graphics.Render
open FSharp.Graphics.Tea
open GlyphAuro

// ─────────────────────────────────────────────────────────────────────────────
// Shared assets
// ─────────────────────────────────────────────────────────────────────────────

let titleFont  = FontFace.SansSerif 24.0f
let bodyFont   = FontFace.SansSerif 16.0f

// ─────────────────────────────────────────────────────────────────────────────
// Demo scene definition
// ─────────────────────────────────────────────────────────────────────────────

/// Build the static demo scene.  The scene contains:
///   • A white background rectangle
///   • A blue rounded-corner card
///   • A drop-shadowed orange circle
///   • A radial-gradient ellipse
///   • A stroked green rectangle
///   • Two text nodes
///   • A nested VStack with three coloured rects
let buildScene () : Scene =

    // ── Background ────────────────────────────────────────────────────────────
    let bg = rectSimple 0.0f 0.0f 480.0f 320.0f Color.White |> Shape

    // ── Blue rounded card ─────────────────────────────────────────────────────
    let card =
        Rectangle(
            Rect.Create(20.0f, 20.0f, 200.0f, 120.0f),
            CornerRadius.All 16.0f,
            Solid (Color.Create(0.18f, 0.42f, 0.78f, 1.0f)),
            None,
            Some (Shadow.Simple(4.0f, 6.0f, 12.0f, Color.Create(0.0f, 0.0f, 0.0f, 0.35f))))
        |> Shape

    // ── Title text ────────────────────────────────────────────────────────────
    let title =
        Text {
            Text          = "FSharp.Graphics"
            Font          = titleFont
            Color         = Color.White
            Align         = TextAlign.Left
            MaxWidth      = ValueSome 180.0f
            MaxHeight     = ValueNone
            LetterSpacing = 0.5f
            LineSpacing   = 1.2f }

    // ── Subtitle ──────────────────────────────────────────────────────────────
    let subtitle =
        Text {
            Text          = "Phase 2-4 Demo"
            Font          = bodyFont
            Color         = Color.Create(0.85f, 0.92f, 1.0f, 1.0f)
            Align         = TextAlign.Left
            MaxWidth      = ValueSome 180.0f
            MaxHeight     = ValueNone
            LetterSpacing = 0.2f
            LineSpacing   = 1.2f }

    // Card contents as a Padding > VStack
    let cardContent =
        Padding(
            Insets.Create(16.0f, 16.0f, 16.0f, 16.0f),
            Stack(Vertical, 8.0f, Alignment.Start,
                  [ title; subtitle ]))

    // ── Shadowed orange circle ────────────────────────────────────────────────
    let orangeCircle =
        Circle(
            Vec2.Create(320.0f, 80.0f), 55.0f,
            Solid (Color.Create(1.0f, 0.55f, 0.0f, 1.0f)),
            Some (Stroke.Solid(Color.White, 3.0f)),
            Some (Shadow.Simple(3.0f, 5.0f, 10.0f, Color.Create(0.0f, 0.0f, 0.0f, 0.3f))))
        |> Shape

    // ── Radial-gradient ellipse ───────────────────────────────────────────────
    let gradientEllipse =
        Ellipse(
            Vec2.Create(120.0f, 220.0f), 90.0f, 50.0f,
            RadialGradient(
                Vec2.Create(120.0f, 220.0f), 90.0f,
                [| { Offset = 0.0f; Color = Color.Create(0.95f, 0.25f, 0.6f, 1.0f) }
                   { Offset = 1.0f; Color = Color.Create(0.35f, 0.0f,  0.8f, 0.5f) } |]),
            None, None)
        |> Shape

    // ── Stroked green rectangle ───────────────────────────────────────────────
    let strokedRect =
        Rectangle(
            Rect.Create(250.0f, 160.0f, 120.0f, 80.0f),
            CornerRadius.Create(8.0f, 8.0f, 24.0f, 4.0f),
            Solid (Color.Create(0.15f, 0.75f, 0.35f, 0.8f)),
            Some (Stroke.Solid(Color.Create(0.05f, 0.45f, 0.15f, 1.0f), 2.5f)),
            None)
        |> Shape

    // ── Mini VStack of coloured bars (tests nested stacks) ───────────────────
    let colourBar c =
        rectSimple 0.0f 0.0f 140.0f 16.0f c |> Shape
    let bars =
        Stack(Vertical, 4.0f, Alignment.Start,
              [ colourBar (Color.Create(1.0f, 0.3f, 0.3f, 1.0f))
                colourBar (Color.Create(0.3f, 0.8f, 0.3f, 1.0f))
                colourBar (Color.Create(0.3f, 0.4f, 1.0f, 1.0f)) ])

    let barsPositioned =
        Transform(Matrix3x2.Translation(260.0f, 240.0f), bars)

    // ── Compose everything ────────────────────────────────────────────────────
    Group [
        bg
        card
        Transform(Matrix3x2.Translation(36.0f, 36.0f), cardContent)
        orangeCircle
        gradientEllipse
        strokedRect
        barsPositioned ]

// ─────────────────────────────────────────────────────────────────────────────
// Layout + encoder exercise
// ─────────────────────────────────────────────────────────────────────────────

let exercisePipeline (scene: Scene) =
    let viewport = Rect.Create(0.0f, 0.0f, 480.0f, 320.0f)

    // 1. Layout pass
    let layoutResult = LayoutEngine.layoutScene scene viewport
    printfn "Layout done – root bounds: (%.0f,%.0f) %.0fx%.0f"
            layoutResult.Bounds.X layoutResult.Bounds.Y
            layoutResult.Bounds.W layoutResult.Bounds.H

    // 2. Diff (first frame – full replace)
    let delta1 = Diff.diffOpt None scene
    printfn "Frame-1 delta: %A" (delta1.GetType().Name)

    // 3. Diff with identical scene (should be NoChange)
    let delta2 = Diff.diffOpt (Some scene) scene
    printfn "Frame-2 delta (same scene): %A" delta2

    // 4. Encode
    let shapeCache = ShapeCache(64)
    let cachedTree = Render.Internal.CachedNode.build scene
    let drawList   = Render.Internal.CachedNode.getDrawList cachedTree shapeCache
    let (buf, len) = Encode.encode drawList
    printfn "Encoded %d commands into %d bytes  (cache entries: %d)"
            drawList.Length len shapeCache.Count

    drawList

// ─────────────────────────────────────────────────────────────────────────────
// TEA round-trip demo (counter)
// ─────────────────────────────────────────────────────────────────────────────

type CounterMsg = Increment | Decrement

let counterProgram : Program<int, CounterMsg> =
    { Init   = fun () -> 0, Cmd.none
      Update = fun model msg ->
                   match msg with
                   | Increment -> model + 1, Cmd.none
                   | Decrement -> model - 1, Cmd.none
      View   = fun count ->
                   let label =
                       Text {
                           Text  = sprintf "Count: %d" count
                           Font  = FontFace.SansSerif 20.0f
                           Color = Color.Black
                           Align = TextAlign.Center
                           MaxWidth  = ValueSome 200.0f
                           MaxHeight = ValueNone
                           LetterSpacing = 0.0f
                           LineSpacing   = 1.0f }
                   Padding(Insets.All 20.0f, label) }

let runTeaDemo () =
    let frames = Runtime.runHeadless counterProgram
                     [ [Increment; Increment]
                       [Increment]
                       [Decrement] ]
    printfn "TEA demo: %d frames rendered, frame sizes: %A"
            frames.Length (frames |> List.map (fun b -> b.Length))

// ─────────────────────────────────────────────────────────────────────────────
// PNG rendering
// ─────────────────────────────────────────────────────────────────────────────

let renderToPng (scene: Scene) (outputPath: string) =
    let w, h = 480, 320
    let canvas = Rasterizer.Canvas(w, h)
    canvas.Clear Color.White

    // Rasterize the scene using the software renderer.
    Rasterizer.rasterizeScene canvas Matrix3x2.Identity scene

    // Encode and write the PNG.
    let png = Png.encode w h canvas.Pixels
    System.IO.File.WriteAllBytes(outputPath, png)
    printfn "PNG written to: %s  (%d bytes)" outputPath png.Length

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// ─────────────────────────────────────────────────────────────────────────────

[<EntryPoint>]
let main _ =
    printfn "=== FSharp.Graphics Demo ==="
    printfn ""

    let scene = buildScene ()

    // Exercise the full Phase 2–4 pipeline
    printfn "--- Pipeline exercise ---"
    let _ = exercisePipeline scene
    printfn ""

    // TEA round-trip
    printfn "--- TEA runtime ---"
    runTeaDemo ()
    printfn ""

    // Render to PNG
    printfn "--- PNG rendering ---"
    let outPath = System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), "demo_output.png")
    renderToPng scene outPath
    printfn ""
    printfn "=== Done ==="
    0
