namespace FSharp.Graphics.GuiBuilder

open System
open FSharp.Math
open FSharp.Graphics.Tea
open FSharp.Graphics.Widgets

type Kind =
    | KButton | KLabel | KHeading | KToggle | KCheck | KSlider | KText | KProgress | KCard | KDivider

module Kind =
    let all = [ KButton; KLabel; KHeading; KToggle; KCheck; KSlider; KText; KProgress; KCard; KDivider ]
    let name = function
        | KButton -> "Button" | KLabel -> "Label" | KHeading -> "Heading" | KToggle -> "Toggle"
        | KCheck -> "CheckBox" | KSlider -> "Slider" | KText -> "TextField" | KProgress -> "Progress"
        | KCard -> "Card" | KDivider -> "Divider"
    let hasText = function KSlider | KProgress | KDivider -> false | _ -> true
    let hasWidth = function KButton | KSlider | KText | KProgress | KDivider -> true | _ -> false
    let hasValue = function KSlider | KProgress -> true | _ -> false
    let hasOn = function KToggle | KCheck -> true | _ -> false
    let defaultText = function
        | KButton -> "Button" | KLabel -> "Label text" | KHeading -> "Heading" | KToggle -> "Toggle me"
        | KCheck -> "Check me" | KText -> "" | KCard -> "Card title" | _ -> ""

/// One element on the design canvas (absolute position; `W` = 0 means "natural width").
type Node =
    { Id: int
      Kind: Kind
      X: float32
      Y: float32
      W: float32
      Text: string
      On: bool
      Value: float32
      Variant: Button.Variant }

type Model =
    { Nodes: Node list
      NextId: int
      Selected: int option
      Ui: UiState
      DarkDesign: bool
      Preview: bool
      ShowCode: bool
      CodeScroll: float32
      VariantOpen: bool }

type Msg =
    | UiMsg of UiMsg
    | Add of Kind
    | Select of int option
    | MoveTo of id: int * x: float32 * y: float32
    | SetW of id: int * w: float32
    | SetText of id: int * string
    | SetOn of id: int * bool
    | SetValue of id: int * float32
    | ToggleVariantMenu
    | SetVariant of id: int * Button.Variant
    | Delete
    | Duplicate
    | BringToFront
    | Key of string
    | ToggleTheme
    | TogglePreview
    | ToggleCode
    | ScrollCode of float32

module Model =
    let variants = [ Button.Primary; Button.Secondary; Button.Danger; Button.Ghost ]
    let snap (v: float32) = MathF.Round(v / 8.0f) * 8.0f

    let newNode (id: int) (kind: Kind) (x: float32) (y: float32) : Node =
        { Id = id; Kind = kind; X = x; Y = y
          W = (match kind with KSlider | KText | KProgress | KDivider -> 220.0f | _ -> 0.0f)
          Text = Kind.defaultText kind; On = (kind = KToggle); Value = 0.5f; Variant = Button.Primary }

    let init () : Model * Cmd<Msg> =
        let mk id kind x y f = f (newNode id kind x y)
        let nodes =
            [ mk 1 KHeading 48.0f 48.0f (fun n -> { n with Text = "Sign in" })
              mk 2 KLabel 48.0f 88.0f (fun n -> { n with Text = "Welcome back. Please enter your details." })
              mk 3 KText 48.0f 128.0f (fun n -> { n with W = 280.0f })
              mk 4 KCheck 48.0f 184.0f (fun n -> { n with Text = "Remember me"; On = true })
              mk 5 KButton 48.0f 224.0f (fun n -> { n with Text = "Sign in"; W = 280.0f }) ]
        { Nodes = nodes; NextId = 6; Selected = Some 5; Ui = UiState.empty
          DarkDesign = true; Preview = false; ShowCode = false; CodeScroll = 0.0f; VariantOpen = false }, Cmd.none

    let private mapNode id f (m: Model) =
        { m with Nodes = m.Nodes |> List.map (fun n -> if n.Id = id then f n else n) }

    let private selectedNode (m: Model) =
        m.Selected |> Option.bind (fun id -> m.Nodes |> List.tryFind (fun n -> n.Id = id))


    let update (m: Model) (msg: Msg) : Model * Cmd<Msg> =
        let m' =
            match msg with
            | UiMsg u -> { m with Ui = UiState.update m.Ui u }
            | Add kind ->
                // cascade new elements so they never stack exactly on top of each other
                let k = float32 (m.Nodes.Length % 8)
                let n = newNode m.NextId kind (snap (80.0f + k * 24.0f)) (snap (80.0f + k * 24.0f))
                { m with Nodes = m.Nodes @ [ n ]; NextId = m.NextId + 1; Selected = Some n.Id }
            | Select s -> { m with Selected = s }
            | MoveTo(id, x, y) -> mapNode id (fun n -> { n with X = max 0.0f x; Y = max 0.0f y }) m
            | SetW(id, w) -> mapNode id (fun n -> { n with W = max 40.0f (snap w) }) m
            | SetText(id, t) -> mapNode id (fun n -> { n with Text = t }) m
            | SetOn(id, v) -> mapNode id (fun n -> { n with On = v }) m
            | SetValue(id, v) -> mapNode id (fun n -> { n with Value = v }) m
            | ToggleVariantMenu -> { m with VariantOpen = not m.VariantOpen }
            | SetVariant(id, v) -> mapNode id (fun n -> { n with Variant = v }) { m with VariantOpen = false }
            | Delete ->
                match m.Selected with
                | Some id -> { m with Nodes = m.Nodes |> List.filter (fun n -> n.Id <> id); Selected = None }
                | None -> m
            | Duplicate ->
                match selectedNode m with
                | Some n ->
                    let c = { n with Id = m.NextId; X = n.X + 16.0f; Y = n.Y + 16.0f }
                    { m with Nodes = m.Nodes @ [ c ]; NextId = m.NextId + 1; Selected = Some c.Id }
                | None -> m
            | BringToFront ->
                match selectedNode m with
                | Some n -> { m with Nodes = (m.Nodes |> List.filter (fun x -> x.Id <> n.Id)) @ [ n ] }
                | None -> m
            | ToggleTheme -> { m with DarkDesign = not m.DarkDesign }
            | TogglePreview -> { m with Preview = not m.Preview; Selected = (if m.Preview then m.Selected else None) }
            | ToggleCode -> { m with ShowCode = not m.ShowCode; CodeScroll = 0.0f }
            | ScrollCode v -> { m with CodeScroll = v }
            | Key key ->
                let order (n: Node) =
                    [ if Kind.hasText n.Kind then yield "insp-text"
                      if n.Kind = KButton then yield "insp-variant"
                      yield "insp-x"; yield "insp-y"
                      if Kind.hasWidth n.Kind then yield "insp-w"
                      if Kind.hasValue n.Kind then yield "insp-v"
                      yield "insp-dup"; yield "insp-front"; yield "insp-del" ]
                match m.Ui.Focus, selectedNode m with
                | Some "insp-text", Some n when key <> "Tab" ->
                    match key with
                    | "Enter" | "Escape" -> { m with Ui = UiState.update m.Ui UiMsg.Blur }
                    | k -> mapNode n.Id (fun n -> { n with Text = TextEdit.apply m.Ui.Shift k n.Text }) m
                | _, Some n when key = "Tab" ->
                    { m with Ui = UiState.cycleFocus (order n) m.Ui.Shift m.Ui }
                // keyboard control of a focused slider: arrows step by 5 % of its range
                | Some f, Some n when (key = "Left" || key = "Right") && f.StartsWith "insp-" && f <> "insp-text" ->
                    let dir = if key = "Left" then -1.0f else 1.0f
                    match f with
                    | "insp-x" -> mapNode n.Id (fun n -> { n with X = max 0.0f (snap (n.X + dir * 8.0f)) }) m
                    | "insp-y" -> mapNode n.Id (fun n -> { n with Y = max 0.0f (snap (n.Y + dir * 8.0f)) }) m
                    | "insp-w" -> mapNode n.Id (fun n -> { n with W = max 40.0f (n.W + dir * 8.0f) }) m
                    | "insp-v" -> mapNode n.Id (fun n -> { n with Value = Scalar.clamp 0.0f 1.0f (n.Value + dir * 0.05f) }) m
                    | _ -> m
                | _, Some n ->
                    let d = 8.0f
                    match key with
                    | "Delete" | "Backspace" -> { m with Nodes = m.Nodes |> List.filter (fun x -> x.Id <> n.Id); Selected = None }
                    | "Left" -> mapNode n.Id (fun n -> { n with X = max 0.0f (n.X - d) }) m
                    | "Right" -> mapNode n.Id (fun n -> { n with X = n.X + d }) m
                    | "Up" -> mapNode n.Id (fun n -> { n with Y = max 0.0f (n.Y - d) }) m
                    | "Down" -> mapNode n.Id (fun n -> { n with Y = n.Y + d }) m
                    | "Escape" -> { m with Selected = None }
                    | _ -> m
                | _ -> m
        m', Cmd.none
