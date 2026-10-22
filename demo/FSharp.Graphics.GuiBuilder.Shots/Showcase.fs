module FSharp.Graphics.GuiBuilder.Showcase

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.SceneDsl
open FSharp.Graphics.Widgets

/// Scene authored purely with the DSL.
let dsl () : Scene =
    let ink = hex 0xF1F5F9
    Group [
        rectangle { bounds 0 0 900 560; fill (hex 0x0F172A) }
        // gradient fills bind to the shape's own bounds - no coordinates repeated
        rectangle { bounds 40 40 260 160; radius 18; vgradient (hex 0xFF6400) (hex 0x6400FF)
                    shadow 0 10 24 (rgba 0 0 0 120) }
        rectangle { bounds 330 40 260 160; radius 18
                    angled 45 [| 0 ==> hex 0x22C55E; 0.5f ==> hex 0x06B6D4; 1 ==> hex 0x3B82F6 |] }
        rectangle { bounds 620 40 240 160; radius 18; hgradient (hex 0xEC4899) (hex 0xF59E0B); stroke ink 2 }
        circle { at 170 330; radius 70; radial [| 0 ==> hex 0xFDE68A; 1 ==> hex 0xF59E0B |]; stroke ink 3 }
        ellipse { bounds 280 270 220 120; fill (hex 0x8B5CF6); rotate 20; opacity 0.85f }
        polygon (starPoints 700.0f 330.0f 80.0f 34.0f 5 0.0f) (Solid (hex 0xFACC15))
        // containers + text
        column {
            at 40 430
            gap 6
            child (text { content "Scene DSL"; size 26; color ink; bold })
            child (text { content "Zero-overhead computation expressions"; size 14; color (hex 0x94A3B8) })
            child (row { gap 10
                         child (circle { radius 8; at 8 8; fill (hex 0x22C55E) })
                         child (text { content "int and float32 both work"; size 13; color ink }) })
        }
    ]

/// Every widget in every visible state (state injected through `UiState`).
let gallery (ui: Ui<unit>) : Scene =
    let th = ui.Theme
    let st (h: string option) (p: string option) (f: string option) = { ui with State = { Hover = h; Pressed = p; Focus = f } }
    let hov = st (Some "b-hover") None None
    let prs = st None (Some "b-press") None
    let foc = st None None (Some "tf")
    let btn (u: Ui<unit>) id v txt dis = Button.view u id { Button.make txt ignore with Variant = v; Disabled = dis }
    let content =
        column {
            gap 14
            child (Label.view ui Label.Title "Widgets")
            child (row { gap 10
                         child (btn ui "b1" Button.Primary "Primary" false)
                         child (btn hov "b-hover" Button.Primary "Hover" false)
                         child (btn prs "b-press" Button.Primary "Pressed" false)
                         child (btn ui "b2" Button.Secondary "Secondary" false)
                         child (btn ui "b3" Button.Danger "Danger" false)
                         child (btn ui "b4" Button.Ghost "Ghost" false)
                         child (btn ui "b5" Button.Primary "Disabled" true) })
            child (row { gap 28
                         child (Toggle.view ui "t1" "Wi-Fi" true ignore)
                         child (Toggle.view ui "t2" "Bluetooth" false ignore)
                         child (CheckBox.view ui "c1" "Accept terms" true ignore)
                         child (CheckBox.view ui "c2" "Subscribe" false ignore) })
            child (Slider.view ui "s1" { Min = 0.0f; Max = 1.0f; Value = 0.35f; Width = 320.0f; OnChange = ignore })
            child (ProgressBar.view ui 320.0f 0.62f)
            child (row { gap 12
                         child (TextField.view ui "tf0" { Value = "Hello"; Placeholder = "Type"; Width = 200.0f })
                         child (TextField.view foc "tf" { Value = "Focused"; Placeholder = "Type"; Width = 200.0f })
                         child (TextField.view ui "tf2" { Value = ""; Placeholder = "Placeholder"; Width = 200.0f }) })
            child (Divider.horizontal ui 640.0f)
            child (Label.view ui Label.Muted "Cards hug their content:")
            child (Panel.view ui Panel.card
                    (column { gap 6
                              child (Label.view ui Label.Heading "Card title")
                              child (Label.view ui Label.Muted "Measured with the real text engine.")
                              child (Button.view ui "cb" (Button.make "Action" ignore)) }))
        }
    Group [ rectangle { bounds 0 0 900 620; fill th.Background }
            shift 32 28 content ]

/// CSG: exact SDF composition, baked to polygons so every backend draws it correctly.
let csg () : Scene =
    let c (x: float32) y r = CircleSdf(Vec2.create x y, r) :> ISdfShape
    let bl = Csg.smooth 28.0f (c 120.0f 110.0f 50.0f) (c 200.0f 130.0f 38.0f)
    let uni = Csg.union (c 380.0f 110.0f 50.0f) (c 430.0f 130.0f 50.0f)
    let sub = Csg.subtract (RectSdf(Rect.create 560.0f 60.0f 140.0f 100.0f, CornerRadius.all 20.0f) :> ISdfShape) (c 670.0f 110.0f 44.0f)
    let inter = Csg.intersect (c 790.0f 110.0f 60.0f) (c 840.0f 110.0f 60.0f)
    let paint = Paint.linearV (Rect.create 0.0f 40.0f 1.0f 140.0f) [| 0 ==> hex 0x60A5FA; 1 ==> hex 0x8B5CF6 |]
    Group [
        rectangle { bounds 0 0 920 240; fill (hex 0x0F172A) }
        Csg.bake bl paint None 1.5f
        Csg.bake uni (Solid (hex 0x22C55E)) None 1.5f
        Csg.bake sub (Solid (hex 0xF59E0B)) None 1.5f
        Csg.bake inter (Solid (hex 0xEC4899)) None 1.5f
        shift 90 196 (label 13 (hex 0x94A3B8) "smooth union")
        shift 380 196 (label 13 (hex 0x94A3B8) "union")
        shift 590 196 (label 13 (hex 0x94A3B8) "subtract")
        shift 780 196 (label 13 (hex 0x94A3B8) "intersect")
    ]
