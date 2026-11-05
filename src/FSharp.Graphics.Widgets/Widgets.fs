namespace FSharp.Graphics.Widgets

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.SceneDsl

/// Every widget is a pure function `(ui, id, props) -> Scene`. The scene's natural top-left is
/// the origin, so widgets drop straight into `column { }` / `row { }`.

module Label =
    type Role = Body | Muted | Caption | Heading | Title

    let view (ui: Ui<'m>) (role: Role) (s: string) : Scene =
        let th = ui.Theme
        let size, color =
            match role with
            | Body -> th.FontSize, th.Text
            | Muted -> th.FontSize, th.TextMuted
            | Caption -> th.FontSize - 2.0f, th.TextMuted
            | Heading -> th.FontSize + 4.0f, th.Text
            | Title -> th.FontSize + 10.0f, th.Text
        Text(s, Ui.textStyle ui size color)

module Divider =
    let horizontal (ui: Ui<'m>) (width: float32) : Scene =
        rectangle { bounds 0.0f 0.0f width 1.0f; fill ui.Theme.Border }
    let vertical (ui: Ui<'m>) (height: float32) : Scene =
        rectangle { bounds 0.0f 0.0f 1.0f height; fill ui.Theme.Border }

module Panel =
    type Props = { Padding: float32; Radius: float32; Fill: Color voption; Border: bool; Shadow: bool }

    let card : Props = { Padding = 14.0f; Radius = 10.0f; Fill = ValueNone; Border = true; Shadow = true }

    /// Background that hugs the (measured) content.
    let view (ui: Ui<'m>) (p: Props) (content: Scene) : Scene =
        let th = ui.Theme
        let inner, size = Ui.normalize ui content
        let w = size.X + p.Padding * 2.0f
        let h = size.Y + p.Padding * 2.0f
        let fillColor = defaultValueArg p.Fill th.Surface
        let bg =
            rectangle {
                bounds 0.0f 0.0f w h
                radius p.Radius
                fill fillColor
            }
        let bg =
            match bg with
            | Shape sh ->
                let sh1 = if p.Border then Shapes.withStroke (Stroke.solid th.Border 1.0f) sh else sh
                let sh2 = if p.Shadow then Shapes.withShadow (Shadow.simple 0.0f 6.0f 18.0f (Color.rgba 0uy 0uy 0uy 90uy)) sh1 else sh1
                Shape sh2
            | other -> other
        Group [ bg; Scene.translate p.Padding p.Padding inner ]

module Button =
    type Variant = Primary | Secondary | Danger | Ghost

    type Props =
        { Text: string
          Variant: Variant
          Disabled: bool
          Width: float32 voption
          OnClick: unit -> unit }

    let make (text: string) (onClick: unit -> unit) : Props =
        { Text = text; Variant = Primary; Disabled = false; Width = ValueNone; OnClick = onClick }

    let view (ui: Ui<'m>) (id: string) (p: Props) : Scene =
        let th = ui.Theme
        let hover, pressed = Ui.isHover ui id, Ui.isPressed ui id
        let bg, fg =
            match p.Variant with
            | Primary -> (if pressed then th.AccentPressed elif hover then th.AccentHover else th.Accent), th.OnAccent
            | Danger -> (if pressed then hex 0xB91C1C elif hover then hex 0xF87171 else th.Danger), th.OnAccent
            | Secondary -> (if pressed then th.Border elif hover then th.SurfaceRaised else th.Surface), th.Text
            | Ghost -> (if pressed then th.SurfaceRaised elif hover then th.Surface else Color.Transparent), th.Text
        let tw = Ui.textWidth ui th.FontSize p.Text
        let w = match p.Width with ValueSome w -> w | ValueNone -> tw + th.PadX * 2.0f
        let h = th.ControlHeight
        let th_ = th.FontSize * 1.2f
        let body =
            Group [
                (let r = rectangle { bounds 0.0f 0.0f w h; radius th.Radius; fill bg }
                 match r with
                 | Shape sh when Ui.isFocused ui id -> Shape (Shapes.withStroke (Stroke.solid th.AccentHover 2.0f) sh)
                 | Shape sh when p.Variant = Secondary -> Shape (Shapes.withStroke (Stroke.solid th.Border 1.0f) sh)
                 | other -> other)
                Scene.translate ((w - tw) * 0.5f) ((h - th_) * 0.5f) (Text(p.Text, Ui.textStyle ui th.FontSize fg))
            ]
        if p.Disabled then Opacity(0.45f, body) else Ui.interactive ui id p.OnClick body

module CheckBox =
    let view (ui: Ui<'m>) (id: string) (label: string) (value: bool) (onChange: bool -> unit) : Scene =
        let th = ui.Theme
        let hover = Ui.isHover ui id
        let box =
            rectangle {
                bounds 0.0f 0.0f 18.0f 18.0f
                radius 5.0f
                fill (if value then th.Accent elif hover then th.SurfaceRaised else th.Surface)
                stroke (if value then th.Accent else th.Border) 1.5f
            }
        let tick =
            if value then
                Group [ Shape(Path(LineSdf(Vec2.create 4.5f 9.5f, Vec2.create 7.5f 12.5f, 2.0f), Solid th.OnAccent, None, None))
                        Shape(Path(LineSdf(Vec2.create 7.5f 12.5f, Vec2.create 13.5f 5.5f, 2.0f), Solid th.OnAccent, None, None)) ]
            else Empty
        let text = Scene.translate 28.0f 0.0f (Text(label, Ui.textStyle ui th.FontSize th.Text))
        Ui.interactive ui id (fun () -> onChange (not value)) (Group [ box; tick; text ])

module Toggle =
    let view (ui: Ui<'m>) (id: string) (label: string) (value: bool) (onChange: bool -> unit) : Scene =
        let th = ui.Theme
        let hover = Ui.isHover ui id
        let track =
            rectangle {
                bounds 0.0f 0.0f 38.0f 20.0f
                radius 10.0f
                fill (if value then (if hover then th.AccentHover else th.Accent) else (if hover then th.Border else th.SurfaceRaised))
            }
        let knob =
            circle {
                at (if value then 28.0f else 10.0f) 10.0f
                radius 7.0f
                fill th.OnAccent
            }
        let text = Scene.translate 48.0f 1.0f (Text(label, Ui.textStyle ui th.FontSize th.Text))
        Ui.interactive ui id (fun () -> onChange (not value)) (Group [ track; knob; text ])

module ProgressBar =
    let view (ui: Ui<'m>) (width: float32) (value: float32) : Scene =
        let th = ui.Theme
        let v = Scalar.clamp 0.0f 1.0f value
        Group [
            rectangle { bounds 0.0f 0.0f width 8.0f; radius 4.0f; fill th.SurfaceRaised }
            (if v > 0.0f then rectangle { bounds 0.0f 0.0f (width * v) 8.0f; radius 4.0f; fill th.Accent } else Empty)
        ]

module Slider =
    type Props = { Min: float32; Max: float32; Value: float32; Width: float32; OnChange: float32 -> unit }

    /// Pointer positions arrive in the slider's own local space, so the track maps directly to the
    /// value: clicking jumps, dragging follows the pointer (captured until release).
    let view (ui: Ui<'m>) (id: string) (p: Props) : Scene =
        let th = ui.Theme
        let range = Scalar.max' (p.Max - p.Min) 1e-6f
        let t = Scalar.clamp 0.0f 1.0f ((p.Value - p.Min) / range)
        let usable = p.Width - 16.0f
        let thumbX = 8.0f + usable * t
        let hover, pressed = Ui.isHover ui id, Ui.isPressed ui id
        let body =
            Group [
                rectangle { bounds 0.0f 8.0f p.Width 4.0f; radius 2.0f; fill th.SurfaceRaised }
                rectangle { bounds 0.0f 8.0f thumbX 4.0f; radius 2.0f; fill th.Accent }
                circle {
                    at thumbX 10.0f
                    radius (if pressed then 9.0f else 8.0f)
                    fill (if hover || pressed then th.AccentHover else th.Accent)
                    stroke th.Background 2
                }
                // transparent hit area so the whole 20px band is draggable
                rectangle { bounds 0.0f 0.0f p.Width 20.0f; fill Color.Transparent }
            ]
        let dragging = ref false
        let valueAt (x: float32) = Scalar.clamp p.Min p.Max (p.Min + (x - 8.0f) / Scalar.max' usable 1.0f * range)
        Reactive(
            (fun e ->
                match e with
                | PointerEnter _ -> ui.Send(ui.Lift(UiMsg.Hover id)); true
                | PointerLeave _ -> ui.Send(ui.Lift(UiMsg.Unhover id)); true
                | PointerDown(pt, 0) ->
                    dragging.Value <- true
                    ui.Send(ui.Lift(UiMsg.Press id))
                    ui.Send(ui.Lift(UiMsg.Focus id))
                    p.OnChange (valueAt pt.X)          // click on the track jumps there
                    true
                | PointerMove pt when dragging.Value -> p.OnChange (valueAt pt.X); true
                | PointerUp _ when dragging.Value ->
                    dragging.Value <- false
                    ui.Send(ui.Lift UiMsg.Release)
                    true
                | _ -> false),
            body)

module TextField =
    type Props = { Value: string; Placeholder: string; Width: float32 }

    let view (ui: Ui<'m>) (id: string) (p: Props) : Scene =
        let th = ui.Theme
        let focused = Ui.isFocused ui id
        let h = th.ControlHeight
        let box =
            rectangle {
                bounds 0.0f 0.0f p.Width h
                radius th.Radius
                fill th.Background
                stroke (if focused then th.Accent else th.Border) (if focused then 2.0f else 1.0f)
            }
        let shown = if p.Value.Length = 0 then p.Placeholder else p.Value
        let color = if p.Value.Length = 0 then th.TextMuted else th.Text
        let ty = (h - th.FontSize * 1.2f) * 0.5f
        let label = Scene.translate 10.0f ty (Text(shown, Ui.textStyle ui th.FontSize color))
        let caretOn = focused && (int (ui.Time / 500.0f)) % 2 = 0
        let caret =
            if caretOn then
                let x = 10.0f + Ui.textWidth ui th.FontSize p.Value + 1.0f
                rectangle { bounds x (ty - 1.0f) 1.5f (th.FontSize * 1.2f + 2.0f); fill th.Text }
            else Empty
        let clipped = Clip(Rect.create 0.0f 0.0f p.Width h, Group [ label; caret ])
        Ui.interactive ui id ignore (Group [ box; clipped ])

module ScrollView =
    type Props = { Width: float32; Height: float32; Offset: float32; OnScroll: float32 -> unit }

    let view (ui: Ui<'m>) (p: Props) (content: Scene) : Scene =
        let th = ui.Theme
        let inner, size = Ui.normalize ui content
        let maxOff = Scalar.max' 0.0f (size.Y - p.Height)
        let off = Scalar.clamp 0.0f maxOff p.Offset
        let bar =
            if maxOff > 0.0f then
                let barH = Scalar.max' 24.0f (p.Height * p.Height / size.Y)
                let barY = (p.Height - barH) * (off / maxOff)
                rectangle { bounds (p.Width - 6.0f) barY 4.0f barH; radius 2.0f; fill (Color.withAlpha 0.55f th.TextMuted) }
            else Empty
        let body =
            Group [
                Clip(Rect.create 0.0f 0.0f p.Width p.Height, Scene.translate 0.0f (-off) inner)
                bar
                rectangle { bounds 0.0f 0.0f p.Width p.Height; fill Color.Transparent }
            ]
        Reactive(
            (fun e ->
                match e with
                | Scroll d -> p.OnScroll (Scalar.clamp 0.0f maxOff (off - d.Y * 36.0f)); true
                | _ -> false),
            body)

module Dropdown =
    type Props =
        { Options: string list
          Selected: int
          Open: bool
          Width: float32
          OnToggle: unit -> unit
          OnSelect: int -> unit }

    /// The open menu is drawn below the header; place the dropdown late in its parent `Group`
    /// so the menu paints (and hit-tests) above siblings.
    let view (ui: Ui<'m>) (id: string) (p: Props) : Scene =
        let th = ui.Theme
        let h = th.ControlHeight
        let ty = (h - th.FontSize * 1.2f) * 0.5f
        let current = match List.tryItem p.Selected p.Options with Some s -> s | None -> ""
        let header =
            Group [
                rectangle { bounds 0.0f 0.0f p.Width h; radius th.Radius; fill th.Background
                            stroke (if p.Open || Ui.isFocused ui id then th.Accent else th.Border) 1.5f }
                shift 10.0f ty (Text(current, Ui.textStyle ui th.FontSize th.Text))
                // chevron
                Group [ Shape(Path(LineSdf(Vec2.create (p.Width - 22.0f) (h * 0.5f - 2.0f), Vec2.create (p.Width - 16.0f) (h * 0.5f + 3.0f), 1.8f), Solid th.TextMuted, None, None))
                        Shape(Path(LineSdf(Vec2.create (p.Width - 16.0f) (h * 0.5f + 3.0f), Vec2.create (p.Width - 10.0f) (h * 0.5f - 2.0f), 1.8f), Solid th.TextMuted, None, None)) ]
            ]
        let menu =
            if not p.Open then Empty
            else
                let rows =
                    p.Options
                    |> List.mapi (fun i o ->
                        let rid = sprintf "%s/%d" id i
                        let hot = Ui.isHover ui rid
                        let row =
                            Group [
                                rectangle { bounds 0.0f 0.0f (p.Width - 8.0f) 30.0f; radius 6.0f
                                            fill (if hot then th.Accent elif i = p.Selected then th.SurfaceRaised else Color.Transparent) }
                                shift 10.0f 5.0f (Text(o, Ui.textStyle ui th.FontSize (if hot then th.OnAccent else th.Text)))
                            ]
                        shift 4.0f (4.0f + float32 i * 30.0f) (Ui.interactive ui rid (fun () -> p.OnSelect i) row))
                let mh = float32 p.Options.Length * 30.0f + 8.0f
                shift 0.0f (h + 4.0f)
                    (Group [ rectangle { bounds 0.0f 0.0f p.Width mh; radius th.Radius; fill th.Surface
                                         stroke th.Border 1; shadow 0 8 18 (rgba 0 0 0 110) }
                             Group rows ])
        Group [ Ui.interactive ui id p.OnToggle header; menu ]
