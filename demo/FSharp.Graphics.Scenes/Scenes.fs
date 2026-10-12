/// Feature-test scenes. Each one targets a feature area so a regression is obvious at a glance:
/// shapes, strokes, gradients, shadows, transforms, layout, clipping, layers, text,
/// animation, interaction, plus a composed dashboard and a stress test.
module FSharp.Graphics.Scenes.Scenes

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.Scenes.Kit

// ── 1. shapes ──────────────────────────────────────────────────────────────

let shapes (ctx: Ctx) =
    let row1 =
        [ box 0.0f 0.0f 90.0f 70.0f blue
          rbox 0.0f 0.0f 90.0f 70.0f 16.0f green
          rbox 0.0f 0.0f 110.0f 44.0f 22.0f amber
          Shape (Shapes.roundedRect (rc 0.0f 0.0f 90.0f 70.0f) (CornerRadius.create 30.0f 4.0f 30.0f 4.0f) (Solid pink))
          dot 40.0f 35.0f 35.0f violet
          Shape (Shapes.ellipse (v 55.0f 35.0f) 55.0f 28.0f (Solid cyan)) ]
    let row2 =
        [ ngon 40.0f 40.0f 38.0f 3 0.0f; ngon 40.0f 40.0f 38.0f 5 0.0f; ngon 40.0f 40.0f 38.0f 6 0.0f
          star 40.0f 40.0f 40.0f 17.0f 5 0.0f; star 40.0f 40.0f 40.0f 25.0f 8 0.0f; ngon 40.0f 40.0f 40.0f 4 0.0f ]
        |> List.mapi (fun i pts -> Shape (polygon pts (Solid ([| red; amber; green; cyan; blue; violet |].[i]))))
    let lines =
        [ for i, t in [ 1.0f; 2.0f; 4.0f; 8.0f; 14.0f ] |> List.indexed ->
            Shape (Path(LineSdf(v 0.0f (float32 i * 26.0f), v 150.0f (float32 i * 26.0f + 14.0f), t), Solid ink, None, None)) ]
    let dots = [ for i, r in [ 0.5f; 1.0f; 1.5f; 2.5f; 4.0f; 6.0f ] |> List.indexed -> dot (float32 i * 22.0f + 8.0f) 8.0f r ink ]
    // hairlines: pixel-aligned vs half-pixel offset must look the same weight after AA
    let hair = [ for i in 0 .. 5 -> box (float32 i * 18.0f + (if i % 2 = 1 then 0.5f else 0.0f)) 0.0f 1.0f 40.0f ink ]
    Group [
        page ctx
        title ctx "Shapes" "rect · rounded · pill · per-corner radii · circle · ellipse · polygons · lines · sub-pixel AA"
        Group (row1 |> List.mapi (fun i s -> at (24.0f + float32 i * 126.0f) 90.0f s))
        Group (row2 |> List.mapi (fun i s -> at (24.0f + float32 i * 126.0f) 200.0f s))
        at 24.0f 330.0f (Group lines)
        at 230.0f 352.0f (Group dots)
        at 230.0f 392.0f (Group hair)
        caption ctx 230.0f 330.0f "radius 0.5 … 6 (AA of tiny discs)"
        caption ctx 230.0f 436.0f "1px lines at .0 and .5 offsets"
        at 24.0f 480.0f (label ctx 12.0f muted "Rect/RoundedRect SDF are exact; the old version drew half-transparent fills and edge-less strokes.")
    ]

// ── 2. strokes ─────────────────────────────────────────────────────────────

let strokes (ctx: Ctx) =
    let st w = Stroke.solid ink w
    let outline sh w = Shape (Shapes.withStroke (st w) sh)
    let none = Solid Color.Transparent
    let widths = [ for i, w in [ 1.0f; 2.0f; 4.0f; 8.0f ] |> List.indexed -> at (float32 i * 110.0f) 0.0f (outline (Shapes.rect (rc 0.0f 0.0f 90.0f 60.0f) none) w) ]
    let joins =
        [ outline (Shapes.rect (rc 0.0f 0.0f 90.0f 60.0f) none) 10.0f
          Shape (Shapes.withStroke (Stroke.withJoin RoundJoin (st 10.0f)) (Shapes.rect (rc 0.0f 0.0f 90.0f 60.0f) none))
          outline (Shapes.roundedRect (rc 0.0f 0.0f 90.0f 60.0f) (CornerRadius.all 24.0f) none) 10.0f
          outline (Shapes.roundedRect (rc 0.0f 0.0f 90.0f 60.0f) (CornerRadius.all 8.0f) (Solid (alpha 0.35f blue))) 6.0f ]
    let curved =
        [ outline (Shapes.circle (v 45.0f 45.0f) 40.0f none) 2.0f
          outline (Shapes.circle (v 45.0f 45.0f) 40.0f none) 10.0f
          outline (Shapes.ellipse (v 60.0f 45.0f) 58.0f 36.0f none) 4.0f
          Shape (Shapes.withStroke (Stroke.solid amber 4.0f) (Path(PolygonSdf(Array.ofList (star 45.0f 45.0f 44.0f 18.0f 5 0.0f)), Solid (alpha 0.3f amber), None, None)))
          Shape (Shapes.withStroke (Stroke.solid green 3.0f) (Shapes.circle (v 45.0f 45.0f) 40.0f (Solid (alpha 0.4f green)))) ]
    Group [
        page ctx
        title ctx "Strokes" "widths · miter / round joins · inner radius · circle · ellipse · polygon · fill + stroke"
        at 30.0f 110.0f (Group widths)
        Group (joins |> List.mapi (fun i s -> at (30.0f + float32 i * 130.0f) 230.0f s))
        Group (curved |> List.mapi (fun i s -> at (24.0f + float32 i * 150.0f) 340.0f s))
        caption ctx 30.0f 195.0f "widths 1 · 2 · 4 · 8  (centred on the edge)"
        caption ctx 30.0f 310.0f "miter (sharp) · round join · r=24 · translucent fill + 6px"
    ]

// ── 3. gradients ───────────────────────────────────────────────────────────

let gradients (ctx: Ctx) =
    let two a b = stops [ 0.0f, a; 1.0f, b ]
    let rainbow = stops [ 0.0f, red; 0.2f, amber; 0.4f, green; 0.6f, cyan; 0.8f, blue; 1.0f, violet ]
    let cell i (build: Rect -> Scene) =
        let x = 24.0f + float32 (i % 4) * 190.0f
        let y = 90.0f + float32 (i / 4) * 150.0f
        at x y (build (rc 0.0f 0.0f 170.0f 110.0f))
    let sw (mk: Rect -> Paint) (r: Rect) = Shape (Shapes.roundedRect r (CornerRadius.all 12.0f) (mk r))
    let checker (r: Rect) =
        Group [ for i in 0 .. 16 do 
                    for j in 0 .. 10 do
                        if (i + j) % 2 = 0 then 
                            yield box (r.X + float32 i * 10.0f) (r.Y + float32 j * 10.0f) 10.0f 10.0f (hex 0x475569) ]
    Group [
        page ctx
        title ctx "Gradients" "linear H/V/angled · radial · multi-stop · alpha stops · gradient strokes · under rotation"
        cell 0 (sw (fun r -> Paint.linearH r (two blue pink)))
        cell 1 (sw (fun r -> Paint.linearV r (two amber red)))
        cell 2 (sw (fun r -> Paint.linearAngle r 45.0f (two green cyan)))
        cell 3 (sw (fun r -> Paint.linearAngle r 135.0f (two violet amber)))
        cell 4 (sw (fun r -> Paint.radialIn r (two ink blue)))
        cell 5 (sw (fun r -> Paint.radial (v (r.X + 55.0f) (r.Y + 40.0f)) 90.0f (stops [ 0.0f, hex 0xFFFFFF; 0.25f, amber; 1.0f, hex 0x7C2D12 ])))
        cell 6 (sw (fun r -> Paint.linearH r rainbow))
        cell 7 (fun r -> Group [ checker r; sw (fun r -> Paint.linearH r (two (alpha 0.0f blue) blue)) r ])
        cell 8 (fun r -> Shape (Shapes.withStroke { Stroke.create (Paint.linearH r (two pink cyan)) 10.0f with Join = RoundJoin } (Shapes.roundedRect r (CornerRadius.all 20.0f) (Solid Color.Transparent))))
        cell 9 (fun r -> Scene.rotate 20.0f (Rect.center r) (sw (fun r -> Paint.linearH r (two green blue)) r))
        cell 10 (fun r -> Shape (Shapes.circle (Rect.center r) 52.0f (Paint.radialIn (Rect.expand -3.0f (rc (r.X + 35.0f) (r.Y + 5.0f) 100.0f 100.0f)) (stops [ 0.0f, hex 0xFFFFFF; 0.3f, blue; 1.0f, hex 0x1E3A8A ]))))
        cell 11 (fun r -> Shape (Shapes.ellipse (Rect.center r) 80.0f 50.0f (Paint.linearV r (stops [ 0.0f, red; 0.5f, amber; 0.5f, green; 1.0f, blue ]))))
        caption ctx 24.0f 560.0f "hard stop (0.5 / 0.5) · alpha ramp over checker · gradient rotated 20° with its container"
    ]

// ── 4. shadows ─────────────────────────────────────────────────────────────

let shadows (ctx: Ctx) =
    let light = hex 0xE2E8F0
    let card x y blur off spread (col: Color) =
        at x y (Shape (Shapes.roundedRect (rc 0.0f 0.0f 130.0f 80.0f) (CornerRadius.all 14.0f) (Solid Color.White)
                       |> Shapes.withShadow (Shadow.dropSpread off blur spread col)))
    let dark = alpha 0.45f Color.Black
    Group [
        box 0.0f 0.0f ctx.Size.X ctx.Size.Y light
        at 24.0f 18.0f (label ctx 22.0f (hex 0x0F172A) "Shadows")
        at 24.0f 46.0f (label ctx 13.0f (hex 0x64748B) "blur levels · offsets · spread · coloured glow · inner · on circles / ellipses")
        card 40.0f 100.0f 4.0f (v 0.0f 2.0f) 0.0f dark
        card 210.0f 100.0f 12.0f (v 0.0f 6.0f) 0.0f dark
        card 380.0f 100.0f 24.0f (v 0.0f 12.0f) 0.0f dark
        card 550.0f 100.0f 48.0f (v 0.0f 24.0f) 0.0f dark
        card 40.0f 240.0f 16.0f (v 12.0f 12.0f) 0.0f dark
        card 210.0f 240.0f 16.0f (v 0.0f 8.0f) 10.0f dark
        card 380.0f 240.0f 16.0f (v 0.0f 8.0f) -8.0f dark
        card 550.0f 240.0f 30.0f (v 0.0f 0.0f) 2.0f (alpha 0.8f blue)
        at 40.0f 390.0f (Shape (Shapes.roundedRect (rc 0.0f 0.0f 130.0f 80.0f) (CornerRadius.all 14.0f) (Solid (hex 0xCBD5E1))
                                 |> Shapes.withShadow (Shadow.inner (v 0.0f 5.0f) 14.0f (alpha 0.55f Color.Black))))
        at 210.0f 390.0f (Shape (Shapes.circle (v 65.0f 40.0f) 40.0f (Solid Color.White) |> Shapes.withShadow (Shadow.drop (v 0.0f 8.0f) 18.0f dark)))
        at 380.0f 390.0f (Shape (Shapes.ellipse (v 65.0f 40.0f) 62.0f 34.0f (Solid amber) |> Shapes.withShadow (Shadow.drop (v 0.0f 10.0f) 20.0f (alpha 0.55f red))))
        at 550.0f 390.0f (Shape (Path(PolygonSdf(Array.ofList (star 65.0f 42.0f 46.0f 20.0f 5 0.0f)), Solid violet, None, Some (Shadow.drop (v 0.0f 8.0f) 16.0f dark))))
        at 40.0f 500.0f (label ctx 12.0f (hex 0x64748B) "blur 4 · 12 · 24 · 48   |   offset (12,12) · spread +10 · spread −8 · blue glow   |   inner · circle · ellipse · star")
    ]

// ── 5. transforms ──────────────────────────────────────────────────────────

let transforms (ctx: Ctx) =
    let skew k = Scene.Transform({ M11 = 1.0f; M12 = 0.0f; M21 = k; M22 = 1.0f; M31 = 0.0f; M32 = 0.0f }, Empty)
    let sq col = rbox -30.0f -30.0f 60.0f 60.0f 8.0f col
    let fan =
        [ for i in 0 .. 7 ->
            Scene.rotate (float32 i * 45.0f) Vec2.zero (Group [ rbox 10.0f -6.0f 60.0f 12.0f 6.0f (Color.lerp blue pink (float32 i / 7.0f)) ]) ]
    let sk k = Scene.Transform({ M11 = 1.0f; M12 = 0.0f; M21 = k; M22 = 1.0f; M31 = 0.0f; M32 = 0.0f }, rbox -30.0f -30.0f 60.0f 60.0f 8.0f amber)
    ignore skew
    Group [
        page ctx
        title ctx "Transforms" "rotate · scale · skew · mirror · nesting · AA of rotated edges / strokes / shadows"
        at 100.0f 170.0f (Group fan)
        at 260.0f 170.0f (Scene.scale 1.0f 1.0f (sq green))
        at 350.0f 170.0f (Scene.scale 1.5f 1.5f (sq green))
        at 470.0f 170.0f (Scene.scale 2.0f 0.6f (sq green))
        at 600.0f 170.0f (sk 0.6f)
        at 700.0f 170.0f (Scene.scale -1.0f 1.0f (Scene.Transform({ M11 = 1.0f; M12 = 0.0f; M21 = 0.5f; M22 = 1.0f; M31 = 0.0f; M32 = 0.0f }, sq violet)))
        // rotated rounded rect with stroke + shadow: edges must stay smooth, shadow must rotate along
        at 120.0f 360.0f (Scene.rotate 25.0f Vec2.zero
                            (Shape (Shapes.roundedRect (rc -60.0f -35.0f 120.0f 70.0f) (CornerRadius.all 16.0f) (Solid Color.White)
                                    |> Shapes.withStroke (Stroke.solid blue 4.0f)
                                    |> Shapes.withShadow (Shadow.drop (v 0.0f 10.0f) 18.0f (alpha 0.6f Color.Black)))))
        // nested: sun / planet / moon, each local to its parent
        at 400.0f 360.0f (Group [
            dot 0.0f 0.0f 28.0f amber
            Scene.rotate 40.0f Vec2.zero (Scene.translate 90.0f 0.0f (Group [
                dot 0.0f 0.0f 16.0f cyan
                Scene.rotate 120.0f Vec2.zero (Scene.translate 32.0f 0.0f (dot 0.0f 0.0f 6.0f ink)) ])) ])
        at 640.0f 360.0f (Scene.rotate 90.0f Vec2.zero (label ctx 28.0f ink "Rotated text"))
        at 24.0f 520.0f (label ctx 12.0f muted "stack: translate ∘ rotate ∘ scale compose in tree order; rotated text uses the same glyph path")
    ]

// ── 6. layout ──────────────────────────────────────────────────────────────

let layout (ctx: Ctx) =
    let b w h col = rbox 0.0f 0.0f w h 6.0f col
    let heights = [ b 40.0f 30.0f blue; b 40.0f 60.0f green; b 40.0f 45.0f amber; b 40.0f 75.0f pink ]
    let chip col = Scene.padAll 4.0f (b 46.0f 22.0f col)
    Group [
        page ctx
        title ctx "Layout" "hstack / vstack · gap · cross-axis alignment · padding · nesting · zstack"
        // the three bars that used to pile up at the origin
        at 24.0f 90.0f (Scene.vstack 8.0f [ b 120.0f 24.0f red; b 120.0f 24.0f amber; b 120.0f 24.0f green ])
        caption ctx 24.0f 190.0f "vstack gap 8"
        at 180.0f 90.0f (Scene.hstack 10.0f heights)
        caption ctx 180.0f 190.0f "hstack · Start"
        at 380.0f 90.0f (hstackA 10.0f CrossAxisAlignment.Center heights)
        caption ctx 380.0f 190.0f "hstack · Center"
        at 580.0f 90.0f (hstackA 10.0f CrossAxisAlignment.End_ heights)
        caption ctx 580.0f 190.0f "hstack · End"
        // padding: each box is wrapped in 12px padding, so neighbours sit 24px apart
        at 24.0f 230.0f (Scene.hstack 0.0f [ for c in [ blue; green; amber; pink ] -> Scene.padAll 12.0f (b 60.0f 40.0f c) ])
        caption ctx 24.0f 310.0f "padAll 12 around each box → 24px between them"
        // nested card built from stacks only
        at 400.0f 230.0f (
            Group [
                rbox 0.0f 0.0f 340.0f 130.0f 14.0f panel
                Scene.padAll 16.0f (
                    Scene.vstack 10.0f [
                        label ctx 16.0f ink "Nested stacks"
                        Scene.hstack 8.0f [ chip red; chip amber; chip green; chip cyan; chip violet ]
                        label ctx 12.0f muted "no coordinates, only structure" ]) ])
        // zstack overlap
        at 24.0f 350.0f (Scene.zstack [ rbox 0.0f 0.0f 160.0f 100.0f 12.0f blue; rbox 40.0f 30.0f 160.0f 100.0f 12.0f (alpha 0.8f pink) ])
        caption ctx 24.0f 495.0f "zstack: children keep their own origin"
        at 260.0f 350.0f (Scene.hstack 20.0f [ for i in 1 .. 4 -> dot 0.0f 0.0f (float32 i * 8.0f) (Color.lerp cyan violet (float32 i / 4.0f)) ])
        caption ctx 260.0f 495.0f "children are placed by their size, not authored position"
    ]

// ── 7. clipping ────────────────────────────────────────────────────────────

let clipping (ctx: Ctx) =
    let big = Group [ dot 60.0f 60.0f 70.0f (alpha 0.9f violet); box 0.0f 55.0f 200.0f 10.0f ink ]
    Group [
        page ctx
        title ctx "Clipping" "rect clip · nested (intersection) · clip under rotation · clip + transform"
        at 40.0f 100.0f (Scene.Clip(rc 0.0f 0.0f 120.0f 90.0f, big))
        at 40.0f 100.0f (Scene.Clip(rc 0.0f 0.0f 120.0f 90.0f, Shape (Shapes.withStroke (Stroke.solid muted 1.0f) (Shapes.rect (rc 0.0f 0.0f 120.0f 90.0f) (Solid Color.Transparent)))))
        // nested clips: visible area is the intersection
        at 240.0f 100.0f (Scene.Clip(rc 0.0f 0.0f 130.0f 90.0f, Scene.Clip(rc 50.0f 30.0f 130.0f 90.0f, Group [ box -20.0f -20.0f 300.0f 200.0f (alpha 0.9f green) ])))
        // clip is rotated together with the content (clip lives in local space)
        at 520.0f 150.0f (
            let area = rc -100.0f -100.0f 200.0f 200.0f
            Scene.rotate 30.0f Vec2.zero
                (Scene.Clip(rc -60.0f -40.0f 120.0f 80.0f,
                            Shape (Shapes.rect area (Paint.linearH (rc -60.0f 0.0f 120.0f 1.0f) (stops [ 0.0f, amber; 1.0f, red ]))))))
        caption ctx 40.0f 205.0f "plain clip"
        caption ctx 240.0f 205.0f "intersection of two clips"
        caption ctx 460.0f 205.0f "rotated clip — edge stays anti-aliased"
        // text clipped to a box (overflow hidden)
        at 40.0f 280.0f (Scene.Clip(rc 0.0f 0.0f 200.0f 40.0f, label ctx 34.0f ink "Clipped headline text"))
        at 40.0f 330.0f (Scene.Clip(rc 0.0f 0.0f 200.0f 28.0f, paragraph ctx 16.0f muted 180.0f TextAlign.Left "This paragraph is wrapped to the clip width, and anything past the box height is simply cut off by the clip."))
        // clipped gradient cards (scroll-view look)
        at 400.0f 280.0f (Scene.Clip(rc 0.0f 0.0f 330.0f 200.0f,
                                    Group [ for i in 0 .. 9 -> rbox 10.0f (float32 i * 54.0f - 30.0f) 310.0f 44.0f 8.0f (Color.lerp blue pink (float32 i / 9.0f)) ]))
        caption ctx 400.0f 490.0f "scroll-view: content overflowing a clip"
    ]

// ── 8. layers ──────────────────────────────────────────────────────────────

let layers (ctx: Ctx) =
    let grayscale : ColorMatrix = [| 0.2126f; 0.7152f; 0.0722f; 0.0f; 0.0f;  0.2126f; 0.7152f; 0.0722f; 0.0f; 0.0f;  0.2126f; 0.7152f; 0.0722f; 0.0f; 0.0f;  0.0f; 0.0f; 0.0f; 1.0f; 0.0f |]
    let sepia : ColorMatrix = [| 0.393f; 0.769f; 0.189f; 0.0f; 0.0f;  0.349f; 0.686f; 0.168f; 0.0f; 0.0f;  0.272f; 0.534f; 0.131f; 0.0f; 0.0f;  0.0f; 0.0f; 0.0f; 1.0f; 0.0f |]
    let invert : ColorMatrix = [| -1.0f; 0.0f; 0.0f; 0.0f; 1.0f;  0.0f; -1.0f; 0.0f; 0.0f; 1.0f;  0.0f; 0.0f; -1.0f; 0.0f; 1.0f;  0.0f; 0.0f; 0.0f; 1.0f; 0.0f |]
    let pair col1 col2 = Group [ dot 40.0f 40.0f 34.0f col1; dot 70.0f 40.0f 34.0f col2 ]
    let swatch = Group [ rbox 0.0f 0.0f 110.0f 70.0f 10.0f blue; rbox 20.0f 20.0f 70.0f 30.0f 8.0f amber; dot 85.0f 52.0f 14.0f red; dot 25.0f 50.0f 12.0f green ]
    let backdrop = Shape (Shapes.rect (rc 0.0f 0.0f 110.0f 80.0f) (Paint.linearH (rc 0.0f 0.0f 110.0f 80.0f) (stops [ 0.0f, hex 0xF87171; 0.5f, hex 0xFACC15; 1.0f, hex 0x38BDF8 ])))
    let blend mode =
        Group [ backdrop; Scene.blendMode mode (Group [ dot 38.0f 40.0f 26.0f (hex 0x4ADE80); dot 72.0f 40.0f 26.0f (hex 0xA855F7) ]) ]
    Group [
        page ctx
        title ctx "Layers" "group opacity · blur · blend modes · colour matrices (all render to offscreen layers)"
        at 24.0f 90.0f (pair (alpha 0.5f blue) (alpha 0.5f red))
        at 140.0f 90.0f (Scene.opacity 0.5f (pair blue red))
        at 256.0f 90.0f (Scene.opacity 0.5f (Scene.opacity 0.5f (pair blue red)))
        caption ctx 24.0f 175.0f "per-shape alpha"
        caption ctx 140.0f 175.0f "group 0.5"
        caption ctx 256.0f 175.0f "nested ½ × ½"
        at 420.0f 90.0f (Group [ for i, s in [ 0.0f; 2.0f; 5.0f; 10.0f ] |> List.indexed -> at (float32 i * 85.0f) 0.0f (Scene.blur s (Group [ rbox 5.0f 15.0f 60.0f 50.0f 10.0f green; dot 35.0f 40.0f 12.0f ink ])) ])
        caption ctx 420.0f 175.0f "blur σ = 0 · 2 · 5 · 10"
        Group [ for i, (n, m) in [ "Multiply", Multiply; "Screen", Screen; "Overlay", Overlay; "Difference", Difference; "Exclusion", Exclusion; "Darken", Darken; "Lighten", Lighten ] |> List.indexed ->
                    Group [ at (24.0f + float32 i * 108.0f) 230.0f (Scene.scale 0.9f 0.9f (blend m)); caption ctx (24.0f + float32 i * 108.0f) 310.0f n ] ]
        Group [ for i, (n, m) in [ "original", None; "grayscale", Some grayscale; "sepia", Some sepia; "invert", Some invert ] |> List.indexed ->
                    let s = match m with Some mm -> Scene.colorFilter mm swatch | None -> swatch
                    Group [ at (24.0f + float32 i * 150.0f) 360.0f s; caption ctx (24.0f + float32 i * 150.0f) 440.0f n ] ]
        at 640.0f 360.0f (Scene.blur 3.0f (Scene.opacity 0.7f (Scene.colorFilter sepia swatch)))
        caption ctx 640.0f 440.0f "blur ∘ opacity ∘ filter"
    ]

// ── 9. text ────────────────────────────────────────────────────────────────

let text (ctx: Ctx) =
    let sample = "The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs."
    let col x (align: TextAlign) (name: string) =
        at x 330.0f (Group [ at 0.0f -18.0f (label ctx 11.0f amber name)
                             paragraph ctx 15.0f ink 220.0f align sample ])
    Group [
        page ctx
        title ctx "Text" "sizes · colour · alignment · wrapping · letter spacing · line height · unicode"
        Group [ for i, s in [ 10.0f; 14.0f; 18.0f; 24.0f; 32.0f; 44.0f ] |> List.indexed ->
                    at 24.0f (80.0f + [| 0.0f; 18.0f; 40.0f; 68.0f; 100.0f; 140.0f |].[i]) (label ctx s ink (sprintf "Sphinx of black quartz %.0fpx" s)) ]
        at 600.0f 84.0f (label ctx 22.0f blue "Coloured")
        at 600.0f 114.0f (label ctx 22.0f pink "text")
        at 600.0f 144.0f (label ctx 22.0f green "runs")
        at 24.0f 282.0f (Scene.Text("Letter spacing 3px", { TextStyle.defaultStyle ctx.Font with SizePx = 16.0f; Color = ink; LetterSpacing = 3.0f; Wrap = false }))
        col 24.0f TextAlign.Left "LEFT"
        col 274.0f TextAlign.Center "CENTER"
        col 524.0f TextAlign.Right "RIGHT"
        at 24.0f 470.0f (label ctx 18.0f ink "Ünïcödé · ÄÖÜ ß € · → ← ↑ ↓ · ✓ ✗ · ½ ¼ · αβγ ΔΩ")
        at 24.0f 505.0f (Group [ for i, lh in [ 1.0f; 1.4f; 1.8f ] |> List.indexed ->
                                     at (float32 i * 250.0f) 0.0f (Scene.Clip(rc 0.0f 0.0f 220.0f 80.0f, Scene.Text("Line height " + string lh + ": a longer sentence that wraps onto several lines of text", { TextStyle.defaultStyle ctx.Font with SizePx = 13.0f; Color = muted; LineHeight = lh; Wrap = true }))) ])
    ]

// ── 10. animation (pure function of time) ──────────────────────────────────

let animation (ctx: Ctx) =
    let easings : (string * Easing) list =
        [ "Linear", Linear; "EaseIn", EaseIn; "EaseOut", EaseOut; "EaseInOut", EaseInOut
          "CubicBezier overshoot", CubicBezier(v 0.68f -0.55f, v 0.27f 1.55f)
          "CubicBezier ease", CubicBezier(v 0.25f 0.1f, v 0.25f 1.0f)
          "Steps 5", Steps(5, JumpEnd) ]
    let rows =
        [ for i, (name, e) in List.indexed easings ->
            let tr = { Transition.linear 1800 with Easing = e; Loop = Alternate; DelayMs = 200.0f }
            let p = Timeline.progress tr ctx.Time
            let y = 86.0f + float32 i * 40.0f
            Group [ box 190.0f (y + 11.0f) 300.0f 2.0f panel2
                    dot (190.0f + p * 300.0f) (y + 12.0f) 9.0f (Color.lerp blue pink (float32 i / 6.0f))
                    at 24.0f y (label ctx 12.0f muted name) ] ]
    let t = ctx.Time / 1000.0f
    let pulse = Timeline.progress { Transition.easeInOut 900 with Loop = Alternate } ctx.Time
    Group [
        page ctx
        title ctx "Animation" "Timeline.progress: easing curves · loop / alternate · delay — scenes are pure functions of time"
        Group rows
        at 640.0f 150.0f (Group [
            Scene.rotate (t * 90.0f) Vec2.zero (Shape (polygon (star 0.0f 0.0f 50.0f 22.0f 5 0.0f) (Solid amber)))
            Scene.rotate (-t * 160.0f) Vec2.zero (Scene.translate 78.0f 0.0f (dot 0.0f 0.0f 6.0f cyan)) ])
        at 640.0f 300.0f (Scene.scale (0.7f + 0.5f * pulse) (0.7f + 0.5f * pulse) (dot 0.0f 0.0f 40.0f (Color.lerp pink violet pulse)))
        at 640.0f 420.0f (Group [ for i in 0 .. 7 ->
                                    let a = t * 2.0f + float32 i * pi / 4.0f
                                    dot (MathF.Cos a * 60.0f) (MathF.Sin a * 60.0f) (4.0f + float32 i) (alpha (0.3f + 0.7f * float32 i / 7.0f) green) ])
        at 24.0f 400.0f (Group [ for i in 0 .. 11 ->
                                   let h = 20.0f + 60.0f * (0.5f + 0.5f * MathF.Sin(t * 3.0f + float32 i * 0.6f))
                                   rbox (float32 i * 30.0f) (100.0f - h) 22.0f h 5.0f (Color.lerp cyan blue (float32 i / 11.0f)) ])
        at 24.0f 520.0f (label ctx 12.0f muted (sprintf "t = %.2fs" t))
    ]

// ── 11. interaction ────────────────────────────────────────────────────────

module private State =
    let mutable clicks = 0
    let mutable hovered = -1
    let mutable toggled = false
    let mutable dragging = false
    let mutable drag = Vec2.create 560.0f 330.0f

let interaction (ctx: Ctx) =
    let button id x y (name: string) =
        let hot = State.hovered = id
        let body =
            Group [ Shape (Shapes.roundedRect (rc 0.0f 0.0f 150.0f 44.0f) (CornerRadius.all 10.0f) (Solid (if hot then blue else panel2))
                           |> Shapes.withShadow (Shadow.drop (v 0.0f (if hot then 8.0f else 2.0f)) (if hot then 18.0f else 6.0f) (alpha 0.5f Color.Black)))
                    at 16.0f 13.0f (label ctx 15.0f ink name) ]
        at x y (Scene.onHover (fun () -> State.hovered <- id) (fun () -> if State.hovered = id then State.hovered <- -1)
                  (Scene.onClick (fun () -> State.clicks <- State.clicks + 1) body))
    let toggle =
        let on = State.toggled
        at 40.0f 230.0f (Scene.onClick (fun () -> State.toggled <- not State.toggled)
                            (Group [ rbox 0.0f 0.0f 64.0f 32.0f 16.0f (if on then green else panel2)
                                     dot (if on then 48.0f else 16.0f) 16.0f 12.0f Color.White ]))
    let knob =
        Scene.Reactive((fun e ->
                           match e with
                           | PointerDown _ -> State.dragging <- true; true
                           | PointerUp _ -> State.dragging <- false; true
                           | PointerMove p when State.dragging -> State.drag <- p; true
                           | _ -> false),
                       dot State.drag.X State.drag.Y 28.0f (if State.dragging then pink else violet))
    Group [
        page ctx
        title ctx "Interaction" "hit-testing · hover enter/leave · click · toggle · drag with pointer capture (works in any host)"
        button 1 40.0f 100.0f "Hover me"
        button 2 210.0f 100.0f "Click me"
        button 3 380.0f 100.0f "Or me"
        at 40.0f 170.0f (label ctx 16.0f ink (sprintf "clicks: %d    hovered: %s" State.clicks (if State.hovered < 0 then "none" else string State.hovered)))
        toggle
        at 120.0f 238.0f (label ctx 14.0f muted (if State.toggled then "on" else "off"))
        rbox 380.0f 250.0f 320.0f 200.0f 14.0f panel
        at 400.0f 262.0f (label ctx 12.0f muted "drag the knob (stays captured outside the panel)")
        knob
        at 40.0f 480.0f (label ctx 12.0f muted "Handlers are matched by tree path, so closures rebuilt every frame keep their hover state.")
    ]

// ── 12. dashboard (everything together) ────────────────────────────────────

let dashboard (ctx: Ctx) =
    let stat x (name: string) (value: string) (col: Color) =
        at x 96.0f (Group [
            Shape (Shapes.roundedRect (rc 0.0f 0.0f 150.0f 90.0f) (CornerRadius.all 14.0f) (Solid panel)
                   |> Shapes.withShadow (Shadow.drop (v 0.0f 8.0f) 20.0f (alpha 0.4f Color.Black)))
            Shape (Shapes.roundedRect (rc 0.0f 0.0f 6.0f 90.0f) (CornerRadius.create 14.0f 0.0f 0.0f 14.0f) (Solid col))
            at 18.0f 14.0f (label ctx 12.0f muted name)
            at 18.0f 40.0f (label ctx 26.0f ink value) ])
    let bars = [ 42.0f; 70.0f; 55.0f; 90.0f; 64.0f; 110.0f; 80.0f; 96.0f; 120.0f; 100.0f ]
    Group [
        page ctx
        // sidebar
        rbox 0.0f 0.0f 190.0f ctx.Size.Y 0.0f panel
        at 24.0f 24.0f (label ctx 18.0f ink "FSharp.Graphics")
        Group [ for i, n in [ "Overview"; "Analytics"; "Reports"; "Settings" ] |> List.indexed ->
                    at 24.0f (90.0f + float32 i * 44.0f) (Group [
                        (if i = 0 then rbox -10.0f -8.0f 160.0f 34.0f 8.0f (alpha 0.25f blue) else Empty)
                        dot 6.0f 9.0f 6.0f (if i = 0 then blue else muted)
                        at 22.0f 0.0f (label ctx 14.0f (if i = 0 then ink else muted) n) ]) ]
        // header
        at 220.0f 24.0f (label ctx 22.0f ink "Overview")
        at 220.0f 52.0f (label ctx 12.0f muted "last 30 days")
        stat 220.0f "Revenue" "$48.2k" green
        stat 390.0f "Users" "12,480" blue
        stat 560.0f "Errors" "37" red
        // chart card
        at 220.0f 210.0f (Group [
            Shape (Shapes.roundedRect (rc 0.0f 0.0f 490.0f 220.0f) (CornerRadius.all 14.0f) (Solid panel)
                   |> Shapes.withShadow (Shadow.drop (v 0.0f 8.0f) 20.0f (alpha 0.4f Color.Black)))
            at 20.0f 16.0f (label ctx 14.0f ink "Weekly traffic")
            Group [ for i, h in List.indexed bars ->
                      let x = 24.0f + float32 i * 46.0f
                      Shape (Shapes.roundedRect (rc x (190.0f - h) 30.0f h) (CornerRadius.create 6.0f 6.0f 0.0f 0.0f)
                                 (Paint.linearV (rc x (190.0f - h) 30.0f h) (stops [ 0.0f, hex 0x60A5FA; 1.0f, hex 0x1D4ED8 ]))) ]
            Shape (Path(LineSdf(v 24.0f 190.0f, v 470.0f 190.0f, 1.0f), Solid panel2, None, None)) ])
        // list
        at 220.0f 450.0f (Group [ for i, (n, c) in [ "Deploy #482 succeeded", green; "Build queue delayed", amber; "Alert: p95 latency", red ] |> List.indexed ->
                                      at (float32 i * 170.0f) 0.0f (Group [ dot 6.0f 8.0f 5.0f c; at 18.0f 0.0f (label ctx 12.0f ink n) ]) ])
    ]

// ── 13. stress ─────────────────────────────────────────────────────────────

let stress (ctx: Ctx) =
    let t = ctx.Time / 1000.0f
    let cols, rows = 40, 28
    Group [
        page ctx
        Group [ for j in 0 .. rows - 1 do
                  for i in 0 .. cols - 1 ->
                    let k = float32 (i + j) * 0.25f + t * 2.0f
                    let s = 0.55f + 0.4f * MathF.Sin k
                    let c = Color.lerp blue pink (0.5f + 0.5f * MathF.Sin(k * 0.7f))
                    let x = 12.0f + float32 i * 19.0f
                    let y = 40.0f + float32 j * 19.0f
                    Scene.rotate (k * 40.0f) (v (x + 8.0f) (y + 8.0f)) (rbox x y (16.0f * s) (16.0f * s) 4.0f c) ]
        at 12.0f 12.0f (label ctx 14.0f ink (sprintf "%d animated rounded rects, each rotated" (cols * rows)))
    ]

let all : SceneDef list =
    [ { Id = "shapes";      Title = "Shapes";      Covers = "primitives, SDF correctness, AA";            Build = shapes;      Animated = false }
      { Id = "strokes";     Title = "Strokes";     Covers = "widths, joins, ring geometry";               Build = strokes;     Animated = false }
      { Id = "gradients";   Title = "Gradients";   Covers = "linear/radial/multi-stop/alpha";             Build = gradients;   Animated = false }
      { Id = "shadows";     Title = "Shadows";     Covers = "blur, spread, offset, inner";                Build = shadows;     Animated = false }
      { Id = "transforms";  Title = "Transforms";  Covers = "rotate/scale/skew/nesting";                  Build = transforms;  Animated = false }
      { Id = "layout";      Title = "Layout";      Covers = "stacks, padding, alignment (Resolve)";       Build = layout;      Animated = false }
      { Id = "clipping";    Title = "Clipping";    Covers = "clip, nested, rotated, text boxes";          Build = clipping;    Animated = false }
      { Id = "layers";      Title = "Layers";      Covers = "group opacity, blur, blend, colour matrix";  Build = layers;      Animated = false }
      { Id = "text";        Title = "Text";        Covers = "shaping, align, wrap, spacing, unicode";     Build = text;        Animated = false }
      { Id = "animation";   Title = "Animation";   Covers = "easing, timeline, time-driven scenes";       Build = animation;   Animated = true  }
      { Id = "interaction"; Title = "Interaction"; Covers = "hit-testing, hover, click, drag capture";    Build = interaction; Animated = true  }
      { Id = "dashboard";   Title = "Dashboard";   Covers = "all features composed";                      Build = dashboard;   Animated = false }
      { Id = "stress";      Title = "Stress";      Covers = "1,100+ animated rotated nodes";              Build = stress;      Animated = true  } ]
