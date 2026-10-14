/// WASM entry points for FSharp.Graphics.
///
/// Compiler ABI follows docs/COMPILER-HANDOFF.md (sprint-26b):
///   - Imports:  [<Import("name", "module")>] + nativeOnly
///   - Exports:  top-level let bindings; [<CompiledName>] for custom names
///   - No exception handling in the render path
///   - Use byte[] / int[] for managed buffers (no Span<T> until sprint-27)
///
/// JavaScript host glue:
///   imports = {
///     env: {
///       fsg_submit_frame: (ptr, len) => { /* put pixels to canvas */ },
///       fsg_now_ms:       ()         => performance.now(),
///     }
///   }
module FSharp.Graphics.Wasm.WasmEntry

open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.Render
open FSharp.Graphics.Tea

// ─────────────────────────────────────────────────────────────────────────────
// Host imports  (filled in by the JavaScript / WASM glue layer)
// ─────────────────────────────────────────────────────────────────────────────

#if FABLE_COMPILER
open Fable.Core

/// Submit the encoded command buffer to the host renderer.
[<Import("fsg_submit_frame", "env")>]
let private fsgSubmitFrame (ptr: int) (len: int) : unit = nativeOnly

/// Return the current timestamp in milliseconds.
[<Import("fsg_now_ms", "env")>]
let private fsgNowMs () : float = nativeOnly

#else
// ── .NET stubs for testing / CI ──────────────────────────────────────────────

let mutable private lastSubmittedLen : int = 0

let private fsgSubmitFrame (_ptr: int) (len: int) : unit =
    lastSubmittedLen <- len

let private fsgNowMs () : float =
    let ts   = float (System.Diagnostics.Stopwatch.GetTimestamp())
    let freq = float (System.Diagnostics.Stopwatch.Frequency)
    ts / freq * 1000.0

/// Retrieve the last submitted buffer length (test helper, .NET only).
let getLastSubmitLen () = lastSubmittedLen
#endif

// ─────────────────────────────────────────────────────────────────────────────
// Global runtime state
// ─────────────────────────────────────────────────────────────────────────────

let mutable private activeProgram : (unit -> byte[] * int) option = None

// ─────────────────────────────────────────────────────────────────────────────
// Runtime helpers
// ─────────────────────────────────────────────────────────────────────────────

/// Attach a TEA program to the global WASM runtime and return a dispatch function.
let mountProgram<'Model, 'Msg> (program: Program<'Model, 'Msg>) : 'Msg -> unit =
    let rt = Runtime.start program
    activeProgram <- Some (fun () ->
        let (buf, len) = rt.RenderFrame()
        buf, len)
    rt.Dispatch

// ─────────────────────────────────────────────────────────────────────────────
// Exported WASM entry points
// ─────────────────────────────────────────────────────────────────────────────

/// Called by the host once per animation frame.
[<CompiledName("fsg_render_frame")>]
let renderFrame (timestamp: float) : unit =
    ignore timestamp
    match activeProgram with
    | None     -> ()
    | Some run ->
        let (buf, len) = run()
        if len > 0 then
            fsgSubmitFrame 0 len
            ignore buf

/// Dispatch a pointer-down event.
[<CompiledName("fsg_pointer_down")>]
let pointerDown (x: float32) (y: float32) (button: int) : unit =
    ignore (x, y, button)

/// Dispatch a pointer-up event.
[<CompiledName("fsg_pointer_up")>]
let pointerUp (x: float32) (y: float32) (button: int) : unit =
    ignore (x, y, button)

/// Dispatch a pointer-move event.
[<CompiledName("fsg_pointer_move")>]
let pointerMove (x: float32) (y: float32) : unit =
    ignore (x, y)

/// Dispatch a key-down event.
[<CompiledName("fsg_key_down")>]
let keyDown (keyCode: int) : unit =
    ignore keyCode

/// Dispatch a key-up event.
[<CompiledName("fsg_key_up")>]
let keyUp (keyCode: int) : unit =
    ignore keyCode
