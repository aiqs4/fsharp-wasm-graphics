namespace FSharp.Graphics.Tea

open FSharp.Graphics.Core

/// Command type for TEA architecture.
/// Named CmdNone (not None) to avoid shadowing Option.None.
type Cmd<'Msg> =
    | CmdNone
    | Batch    of Cmd<'Msg> list
    | Emit     of 'Msg
    | Timer    of delayMs: int * 'Msg
    | LoadImage of url: string * (ImageId -> 'Msg)
    | LoadFont  of url: string * sizePx: float32 * (FontId -> 'Msg)
    | Http     of url: string * (string -> 'Msg)

/// Command helpers
module Cmd =
    let none : Cmd<'Msg> = CmdNone
    let batch cmds = Batch cmds
    let emit msg = Emit msg
    let timer delayMs msg = Timer(delayMs, msg)
    let loadImage url callback = LoadImage(url, callback)
    let loadFont url sizePx callback = LoadFont(url, sizePx, callback)
    let http url callback = Http(url, callback)

    /// Flatten nested batches into a flat list
    let rec flatten (cmd: Cmd<'Msg>) : Cmd<'Msg> list =
        match cmd with
        | CmdNone -> []
        | Batch cmds -> cmds |> List.collect flatten
        | other -> [ other ]
