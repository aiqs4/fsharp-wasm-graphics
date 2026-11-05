namespace FSharp.Graphics.GuiBuilder

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.SceneDsl
open FSharp.Graphics.Widgets

module View =

    [<Literal>]
    let TopH = 52.0f
    [<Literal>]
    let PaletteW = 190.0f
    [<Literal>]
    let InspectorW = 270.0f
    [<Literal>]
    let CodeH = 230.0f

    // ---- the designed widgets ---------------------------------------------------------------

    let private nodeWidget (ui: Ui<Msg>) (n: Node) : Scene =
        let id = sprintf "n%d" n.Id
        match n.Kind with
        | KButton ->
            Button.view ui id
                { Button.make n.Text ignore with
                    Variant = n.Variant
                    Width = (if n.W > 0.0f then ValueSome n.W else ValueNone) }
        | KLabel -> Label.view ui Label.Body n.Text
        | KHeading -> Label.view ui Label.Heading n.Text
        | KToggle -> Toggle.view ui id n.Text n.On (fun v -> ui.Send(SetOn(n.Id, v)))
        | KCheck -> CheckBox.view ui id n.Text n.On (fun v -> ui.Send(SetOn(n.Id, v)))
        | KSlider ->
            Slider.view ui id
                { Min = 0.0f; Max = 1.0f; Value = n.Value; Width = n.W
                  OnChange = fun v -> ui.Send(SetValue(n.Id, v)) }
        | KText -> TextField.view ui id { Value = n.Text; Placeholder = "Type here"; Width = n.W }
        | KProgress -> ProgressBar.view ui n.W n.Value
        | KCard -> Panel.view ui Panel.card (Label.view ui Label.Heading n.Text)
        | KDivider -> Divider.horizontal ui n.W

    /// Drag-to-move overlay. Pointer events carry window coordinates, so the drag is relative to
    /// the press point; the closure (created at press time) keeps receiving the captured moves.
    let private dragOverlay (ui: Ui<Msg>) (n: Node) (b: Rect) : Scene =
        let down : Vec2 voption ref = ref ValueNone
        let sx, sy = n.X, n.Y
        Reactive(
            (fun e ->
                match e with
                | PointerDown(p, 0) ->
                    down.Value <- ValueSome p
                    ui.Send(Select(Some n.Id))
                    true
                | PointerMove p when down.Value.IsSome ->
                    let d = down.Value.Value
                    ui.Send(MoveTo(n.Id, Model.snap (sx + p.X - d.X), Model.snap (sy + p.Y - d.Y)))
                    true
                | PointerUp _ when down.Value.IsSome ->
                    down.Value <- ValueNone
                    true
                | _ -> false),
            rectangle { bounds b; fill Color.Transparent })

    let private widthHandle (ui: Ui<Msg>) (th: Theme) (n: Node) (b: Rect) : Scene =
        let down : float32 voption ref = ref ValueNone
        let w0 = b.W
        let handle = rectangle { bounds (b.X + b.W + 1.0f) (b.Y + b.H * 0.5f - 9.0f) 8.0f 18.0f; radius 3.0f; fill th.Accent }
        Reactive(
            (fun e ->
                match e with
                | PointerDown(p, 0) -> down.Value <- ValueSome p.X; true
                | PointerMove p when down.Value.IsSome ->
                    ui.Send(SetW(n.Id, w0 + p.X - down.Value.Value)); true
                | PointerUp _ when down.Value.IsSome -> down.Value <- ValueNone; true
                | _ -> false),
            handle)

    let private canvas (chrome: Ui<Msg>) (m: Model) (w: float32) (h: float32) : Scene =
        let th = if m.DarkDesign then Theme.dark else Theme.light
        let dui = { chrome with Theme = th; State = (if m.Preview then m.Ui else UiState.empty) }
        let grid =
            if m.Preview then Empty
            else
                let line = Color.withAlpha 0.18f th.TextMuted
                Group [ for gx in 1 .. int (w / 32.0f) -> rectangle { bounds (gx * 32) 0 1 (int h); fill line }
                        for gy in 1 .. int (h / 32.0f) -> rectangle { bounds 0 (gy * 32) (int w) 1; fill line } ]
        let node (n: Node) : Scene =
            let widget = nodeWidget dui n
            let b0 = Ui.measure dui widget
            let b = Rect.create b0.X b0.Y (Scalar.max' b0.W 24.0f) (Scalar.max' b0.H 18.0f)
            let body =
                if m.Preview then widget
                else
                    let selected = (m.Selected = Some n.Id)
                    Group [
                        widget
                        (if selected then
                            rectangle { bounds (b.X - 4.0f) (b.Y - 4.0f) (b.W + 8.0f) (b.H + 8.0f)
                                        radius 4.0f; stroke th.Accent 1.5f }
                         else Empty)
                        dragOverlay chrome n b
                        (if selected && Kind.hasWidth n.Kind then widthHandle chrome th n b else Empty)
                    ]
            shift n.X n.Y body
        Clip(Rect.create 0.0f 0.0f w h,
             Group [ rectangle { bounds 0.0f 0.0f w h; fill th.Background }
                     grid
                     Group (m.Nodes |> List.map node) ])

    // ---- chrome -----------------------------------------------------------------------------

    let private palette (ui: Ui<Msg>) : Scene =
        let items =
            [ for k in Kind.all ->
                Button.view ui ("pal-" + Kind.name k)
                    { Button.make ("+  " + Kind.name k) (fun () -> ui.Send(Add k)) with
                        Variant = Button.Secondary; Width = ValueSome 158.0f } ]
        column {
            gap 8
            child (Label.view ui Label.Heading "Elements")
            child (Label.view ui Label.Caption "Click to add to the canvas")
            children items
        }

    let private field (ui: Ui<Msg>) (caption: string) (control: Scene) : Scene =
        column { gap 4; child (Label.view ui Label.Caption caption); child control }

    let private inspector (ui: Ui<Msg>) (m: Model) (canvasW: float32) : Scene =
        match m.Selected |> Option.bind (fun id -> m.Nodes |> List.tryFind (fun n -> n.Id = id)) with
        | None ->
            column {
                gap 8
                child (Label.view ui Label.Heading "Inspector")
                child (Label.view ui Label.Muted "Select an element on the canvas.")
                child (Label.view ui Label.Caption "Arrow keys nudge, Delete removes.")
            }
        | Some n ->
            let send = ui.Send
            let sl id (lo: float32) (hi: float32) (v: float32) (f: float32 -> Msg) =
                Slider.view ui id { Min = lo; Max = hi; Value = v; Width = 220.0f; OnChange = fun x -> send (f x) }
            column {
                gap 12
                child (Label.view ui Label.Heading ("Inspector  -  " + Kind.name n.Kind))
                childIf (Kind.hasText n.Kind)
                    (field ui "Text" (TextField.view ui "insp-text" { Value = n.Text; Placeholder = "(empty)"; Width = 220.0f }))
                child (field ui (sprintf "X  (%d)" (int n.X)) (sl "insp-x" 0.0f canvasW n.X (fun v -> MoveTo(n.Id, Model.snap v, n.Y))))
                child (field ui (sprintf "Y  (%d)" (int n.Y)) (sl "insp-y" 0.0f 600.0f n.Y (fun v -> MoveTo(n.Id, n.X, Model.snap v))))
                childIf (Kind.hasWidth n.Kind)
                    (field ui (sprintf "Width  (%d)" (int n.W)) (sl "insp-w" 40.0f 600.0f (max 40.0f n.W) (fun v -> SetW(n.Id, v))))
                childIf (Kind.hasValue n.Kind)
                    (field ui (sprintf "Value  (%.2f)" n.Value) (sl "insp-v" 0.0f 1.0f n.Value (fun v -> SetValue(n.Id, v))))
                childIf (Kind.hasOn n.Kind)
                    (Toggle.view ui "insp-on" "Checked / on" n.On (fun v -> send (SetOn(n.Id, v))))
                child (Divider.horizontal ui 220.0f)
                child (row {
                    gap 8
                    child (Button.view ui "insp-dup" { Button.make "Duplicate" (fun () -> send Duplicate) with Variant = Button.Secondary })
                    child (Button.view ui "insp-front" { Button.make "To front" (fun () -> send BringToFront) with Variant = Button.Secondary })
                })
                childIf (n.Kind = KButton)
                    (field ui "Variant"
                        (Dropdown.view ui "insp-variant"
                            { Options = Model.variants |> List.map (sprintf "%A"); Selected = List.findIndex ((=) n.Variant) Model.variants
                              Open = m.VariantOpen; Width = 220.0f
                              OnToggle = (fun () -> send ToggleVariantMenu)
                              OnSelect = fun i -> send (SetVariant(n.Id, Model.variants.[i])) }))
                child (Button.view ui "insp-del" { Button.make "Delete" (fun () -> send Delete) with Variant = Button.Danger; Width = ValueSome 220.0f })
            }

    let private topBar (ui: Ui<Msg>) (m: Model) (w: float32) : Scene =
        let send = ui.Send
        let buttons =
            row {
                gap 8
                child (Button.view ui "top-preview"
                        { Button.make (if m.Preview then "Preview: on" else "Preview: off") (fun () -> send TogglePreview) with
                            Variant = (if m.Preview then Button.Primary else Button.Secondary) })
                child (Button.view ui "top-code"
                        { Button.make "Export F#" (fun () -> send ToggleCode) with
                            Variant = (if m.ShowCode then Button.Primary else Button.Secondary) })
                child (Button.view ui "top-theme"
                        { Button.make (if m.DarkDesign then "Design: dark" else "Design: light") (fun () -> send ToggleTheme) with
                            Variant = Button.Secondary })
            }
        let bw = (Ui.measure ui buttons).W
        Group [
            rectangle { bounds 0.0f 0.0f w TopH; fill ui.Theme.Surface }
            rectangle { bounds 0.0f (TopH - 1.0f) w 1.0f; fill ui.Theme.Border }
            shift 20 13 (Label.view ui Label.Heading "GUI Builder")
            shift 150 17 (Label.view ui Label.Caption "FSharp.Graphics.Widgets demo")
            shift (w - bw - 20.0f) 9 buttons
        ]

    let private codePanel (ui: Ui<Msg>) (m: Model) (w: float32) : Scene =
        let th = ui.Theme
        let lines =
            Export.generate m
            |> List.map (fun l ->
                let color = if l.TrimStart().StartsWith "open" then th.TextMuted else th.Text
                Text((if l = "" then " " else l), Ui.textStyle ui 12.0f color))
        let body = column { gap 3; children lines }
        Group [
            rectangle { bounds 0.0f 0.0f w CodeH; fill th.SurfaceRaised; shadow 0 -4 12 (rgba 0 0 0 80) }
            rectangle { bounds 0.0f 0.0f w 1.0f; fill th.Border }
            shift 16 10 (Label.view ui Label.Caption "Generated F#  (scroll)")
            shift 16 34 (ScrollView.view ui { Width = w - 32.0f; Height = CodeH - 44.0f; Offset = m.CodeScroll
                                              OnScroll = fun v -> ui.Send(ScrollCode v) } body)
        ]

    /// Whole-window view. `ui` carries the chrome theme, text engine, interaction state and dispatch.
    let view (ui: Ui<Msg>) (size: Vec2) (m: Model) : Scene =
        let w, h = size.X, size.Y
        let ui = { ui with State = m.Ui }
        let canvasW = w - PaletteW - InspectorW
        let canvasH = h - TopH
        let page =
            Group [
                rectangle { bounds 0.0f 0.0f w h; fill ui.Theme.Background }
                shift PaletteW TopH (canvas ui m canvasW canvasH)
                // side panels
                shift 0.0f TopH (Group [ rectangle { bounds 0.0f 0.0f PaletteW canvasH; fill ui.Theme.Surface }
                                         rectangle { bounds (PaletteW - 1.0f) 0.0f 1.0f canvasH; fill ui.Theme.Border }
                                         shift 16 16 (palette ui) ])
                shift (w - InspectorW) TopH (Group [ rectangle { bounds 0.0f 0.0f InspectorW canvasH; fill ui.Theme.Surface }
                                                     rectangle { bounds 0.0f 0.0f 1.0f canvasH; fill ui.Theme.Border }
                                                     shift 16 16 (inspector ui m canvasW) ])
                (if m.ShowCode then shift PaletteW (h - CodeH) (codePanel ui m canvasW) else Empty)
                topBar ui m w
            ]
        Ui.root ui (fun k -> ui.Send(Key k)) page
