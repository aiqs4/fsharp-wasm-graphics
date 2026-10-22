namespace FSharp.Graphics.GuiBuilder

open System
open FSharp.Graphics.Widgets

/// Turns the design into F# source that uses the Widgets library directly.
module Export =

    let private esc (s: string) = s.Replace("\\", "\\\\").Replace("\"", "\\\"")
    let private f (v: float32) = sprintf "%.1ff" v

    let private variant = function
        | Button.Primary -> "Button.Primary" | Button.Secondary -> "Button.Secondary"
        | Button.Danger -> "Button.Danger" | Button.Ghost -> "Button.Ghost"

    let private expr (n: Node) : string =
        let id = sprintf "\"n%d\"" n.Id
        match n.Kind with
        | KButton ->
            let width = if n.W > 0.0f then sprintf "; Width = ValueSome %s" (f n.W) else ""
            sprintf "Button.view ui %s { Button.make \"%s\" ignore with Variant = %s%s }" id (esc n.Text) (variant n.Variant) width
        | KLabel -> sprintf "Label.view ui Label.Body \"%s\"" (esc n.Text)
        | KHeading -> sprintf "Label.view ui Label.Heading \"%s\"" (esc n.Text)
        | KToggle -> sprintf "Toggle.view ui %s \"%s\" %b ignore" id (esc n.Text) n.On
        | KCheck -> sprintf "CheckBox.view ui %s \"%s\" %b ignore" id (esc n.Text) n.On
        | KSlider -> sprintf "Slider.view ui %s { Min = 0.0f; Max = 1.0f; Value = %s; Width = %s; OnChange = ignore }" id (f n.Value) (f n.W)
        | KText -> sprintf "TextField.view ui %s { Value = \"%s\"; Placeholder = \"Type here\"; Width = %s }" id (esc n.Text) (f n.W)
        | KProgress -> sprintf "ProgressBar.view ui %s %s" (f n.W) (f n.Value)
        | KCard -> sprintf "Panel.view ui Panel.card (Label.view ui Label.Heading \"%s\")" (esc n.Text)
        | KDivider -> sprintf "Divider.horizontal ui %s" (f n.W)

    let generate (m: Model) : string list =
        [ yield "open FSharp.Graphics.Scene"
          yield "open FSharp.Graphics.SceneDsl"
          yield "open FSharp.Graphics.Widgets"
          yield ""
          yield "let view (ui: Ui<'Msg>) : Scene ="
          yield "    Group ["
          for n in m.Nodes do
              yield sprintf "        %s |> shift %d %d" (expr n) (int n.X) (int n.Y)
          yield "    ]" ]
