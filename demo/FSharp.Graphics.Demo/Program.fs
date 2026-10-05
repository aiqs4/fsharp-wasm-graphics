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
open FSharp.Graphics.Runtime
open FSharp.Graphics.Shared.DemoScene
open GlyphAuro

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
    let delta1 = Diff.diffOpt Option.None scene
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
    let frames = RuntimeHelpers.runHeadless counterProgram
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
