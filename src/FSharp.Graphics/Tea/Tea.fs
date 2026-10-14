/// TEA (The Elm Architecture) core types for FSharp.Graphics.
namespace FSharp.Graphics.Tea

open FSharp.Graphics
open FSharp.Graphics.Scene

// ─────────────────────────────────────────────────────────────────────────────
// Cmd  (side-effect description)
// ─────────────────────────────────────────────────────────────────────────────

/// Describes a side-effect that the runtime should perform after an update.
/// Commands are pure data – they carry no execution logic themselves.
[<NoComparison>]
type Cmd<'Msg> =
    | None
    | Batch       of Cmd<'Msg> list
    | LoadImage   of url: string   * (ImageId -> 'Msg)
    | LoadFont    of url: string   * size: float32 * (uint64 -> 'Msg)
    | Timer       of delayMs: int  * 'Msg
    | Emit        of 'Msg

module Cmd =
    let none<'Msg> : Cmd<'Msg> = None
    let batch cmds = Batch cmds
    let emit msg   = Emit msg
    let timer ms msg = Timer(ms, msg)
    let loadImage url f = LoadImage(url, f)
    let loadFont  url size f = LoadFont(url, size, f)

    /// Flatten a Cmd tree into a list of leaf commands.
    let rec flatten (cmd: Cmd<'Msg>) : Cmd<'Msg> list =
        match cmd with
        | None        -> []
        | Batch cmds  -> List.collect flatten cmds
        | leaf        -> [leaf]

// ─────────────────────────────────────────────────────────────────────────────
// Program
// ─────────────────────────────────────────────────────────────────────────────

/// A complete TEA program.
type Program<'Model, 'Msg> =
    { /// Produce the initial model and any startup commands.
      Init   : unit -> 'Model * Cmd<'Msg>
      /// Pure state transition – given current model and a message, returns
      /// new model and any side-effect commands.
      Update : 'Model -> 'Msg -> 'Model * Cmd<'Msg>
      /// Pure view – maps the model to an immutable scene description.
      View   : 'Model -> Scene }

// ─────────────────────────────────────────────────────────────────────────────
// Subscriptions (optional)
// ─────────────────────────────────────────────────────────────────────────────

/// A subscription is a side-effectful listener that can feed messages into the
/// TEA loop (e.g. timers, WebSocket events).
type Sub<'Msg> = (('Msg -> unit) -> unit -> unit)
// The inner function receives a dispatch callback and returns an unsubscribe thunk.
