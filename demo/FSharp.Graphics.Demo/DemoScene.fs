module FSharp.Graphics.Shared.DemoScene

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
            Option.None,
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
            Option.None, Option.None)
        |> Shape

    // ── Stroked green rectangle ───────────────────────────────────────────────
    let strokedRect =
        Rectangle(
            Rect.Create(250.0f, 160.0f, 120.0f, 80.0f),
            CornerRadius.Create(8.0f, 8.0f, 24.0f, 4.0f),
            Solid (Color.Create(0.15f, 0.75f, 0.35f, 0.8f)),
            Some (Stroke.Solid(Color.Create(0.05f, 0.45f, 0.15f, 1.0f), 2.5f)),
            Option.None)
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
