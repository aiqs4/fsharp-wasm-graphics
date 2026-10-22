namespace FSharp.Graphics.Widgets

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Scene

/// Transient interaction state shared by all widgets. Lives in the application model.
type UiState =
    { Hover: string option
      Pressed: string option
      Focus: string option }

[<RequireQualifiedAccess>]
type UiMsg =
    | Hover of string
    | Unhover of string
    | Press of string
    | Release
    | Focus of string
    | Blur

module UiState =
    let empty = { Hover = None; Pressed = None; Focus = None }

    let update (s: UiState) (m: UiMsg) : UiState =
        match m with
        | UiMsg.Hover id   -> { s with Hover = Some id }
        | UiMsg.Unhover id -> if s.Hover = Some id then { s with Hover = None } else s
        | UiMsg.Press id   -> { s with Pressed = Some id }
        | UiMsg.Release    -> { s with Pressed = None }
        | UiMsg.Focus id   -> { s with Focus = Some id }
        | UiMsg.Blur       -> { s with Focus = None }

    /// Tab / Shift+Tab over an explicit focus order.
    let cycleFocus (order: string list) (backwards: bool) (s: UiState) : UiState =
        match order with
        | [] -> s
        | _ ->
            let arr = Array.ofList order
            let n = arr.Length
            let next =
                match s.Focus |> Option.bind (fun f -> Array.tryFindIndex ((=) f) arr) with
                | Some i -> arr.[((if backwards then i - 1 else i + 1) % n + n) % n]
                | None -> if backwards then arr.[n - 1] else arr.[0]
            { s with Focus = Some next }

/// Everything a widget needs to draw and to talk back to the application.
/// `Lift` wraps widget-level messages into the app's message type, `Send` dispatches.
type Ui<'Msg> =
    { Theme: Theme
      Text: ITextEngine
      State: UiState
      Time: float32
      Lift: UiMsg -> 'Msg
      Send: 'Msg -> unit }

module Ui =
    let inline private emit (ui: Ui<'m>) (m: UiMsg) = ui.Send(ui.Lift m)

    let isHover (ui: Ui<'m>) id = ui.State.Hover = Some id
    let isPressed (ui: Ui<'m>) id = ui.State.Pressed = Some id
    let isFocused (ui: Ui<'m>) id = ui.State.Focus = Some id

    let textStyle (ui: Ui<'m>) (size: float32) (color: Color) : TextStyle =
        { TextStyle.defaultStyle ui.Theme.Font with SizePx = size; Color = color; Wrap = false }

    /// Natural size of arbitrary content (runs real layout + text shaping).
    let measure (ui: Ui<'m>) (s: Scene) : Rect =
        match Resolve.bounds (Resolve.resolve ui.Text Single.MaxValue s) with
        | Some b -> b
        | None -> Rect.zero

    let textWidth (ui: Ui<'m>) (size: float32) (s: string) : float32 =
        if s.Length = 0 then 0.0f else (ui.Text.Layout(s, textStyle ui size Color.Black, 0.0f)).Width

    /// Move content so its natural top-left is the origin.
    let normalize (ui: Ui<'m>) (s: Scene) : Scene * Vec2 =
        let b = measure ui s
        (if b.X = 0.0f && b.Y = 0.0f then s else Scene.translate (-b.X) (-b.Y) s), Vec2.create b.W b.H

    /// Standard pointer behaviour: hover / press / focus bookkeeping + a click callback.
    let interactive (ui: Ui<'m>) (id: string) (onClick: unit -> unit) (content: Scene) : Scene =
        Reactive(
            (fun e ->
                match e with
                | PointerEnter _ -> emit ui (UiMsg.Hover id); true
                | PointerLeave _ -> emit ui (UiMsg.Unhover id); true
                | PointerDown(_, 0) -> emit ui (UiMsg.Press id); emit ui (UiMsg.Focus id); true
                | PointerUp(_, 0) ->
                    // activation on release (the press target keeps the pointer, so we cannot
                    // tell here whether the pointer is still over the widget)
                    emit ui UiMsg.Release
                    onClick ()
                    true
                | _ -> false),
            content)

    /// Wrap the whole UI once: delivers key events to the app (the pointer is always "over" the
    /// root, so the hover chain always contains it) and clears focus on background clicks.
    let root (ui: Ui<'m>) (onKey: string -> unit) (content: Scene) : Scene =
        Reactive(
            (fun e ->
                match e with
                | KeyDown k -> onKey k; true
                | PointerDown _ -> emit ui UiMsg.Blur; false
                | _ -> false),
            content)

/// Minimal single-line text editing for hosts that only deliver key *names*
/// (Silk.NET style: "A", "Space", "Number1", "Backspace"). Caret is always at the end.
module TextEdit =
    let private punct =
        dict [ "Space", " "; "Period", "."; "Comma", ","; "Minus", "-"; "Slash", "/"; "Semicolon", ";"
               "Apostrophe", "'"; "Equal", "="; "Underscore", "_" ]

    let keyChar (key: string) : string option =
        if key.Length = 1 && Char.IsLetterOrDigit key.[0] then Some (key.ToLowerInvariant())
        elif key.StartsWith "Number" && key.Length = 7 && Char.IsDigit key.[6] then Some (key.Substring 6)
        elif key.StartsWith "Keypad" && key.Length = 7 && Char.IsDigit key.[6] then Some (key.Substring 6)
        else
            match punct.TryGetValue key with
            | true, c -> Some c
            | _ -> None

    let apply (key: string) (value: string) : string =
        match key with
        | "Backspace" -> if value.Length > 0 then value.Substring(0, value.Length - 1) else value
        | k -> match keyChar k with Some c -> value + c | None -> value
