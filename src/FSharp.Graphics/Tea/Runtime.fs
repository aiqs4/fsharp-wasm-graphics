/// TEA Runtime – the single mutable loop that drives a TEA Program.
///
/// This is the ONLY impure module in FSharp.Graphics; all other modules are
/// pure functions.  In the WASM target the exported functions are called by
/// the host; on the desktop they can be driven by a test harness or a native
/// event loop.
namespace FSharp.Graphics.Runtime

open FSharp.Graphics
open FSharp.Graphics.Scene
open FSharp.Graphics.Render
open FSharp.Graphics.Tea

// ─────────────────────────────────────────────────────────────────────────────
// Input decoder
// ─────────────────────────────────────────────────────────────────────────────

module InputDecoder =

    // Simple binary format (matching the WASM ABI):
    //   event_type : uint8
    //   x, y       : float32  (for pointer events)
    //   button     : uint8    (for mouse events)
    //   key_len    : uint16   (for key/text events)
    //   key_data   : utf-8 bytes

    let private readFloat32 (data: byte[]) (off: int) =
        let bits =
            int data.[off]
            ||| (int data.[off+1] <<< 8)
            ||| (int data.[off+2] <<< 16)
            ||| (int data.[off+3] <<< 24)
        System.BitConverter.Int32BitsToSingle bits

    let private readUInt16 (data: byte[]) (off: int) =
        uint16 data.[off] ||| (uint16 data.[off+1] <<< 8)

    let decode (data: byte[]) : InputEvent option =
        if data.Length = 0 then Option.None
        else
        match data.[0] with
        | 0x01uy ->   // MouseMove
            if data.Length < 9 then Option.None
            else
                let x = readFloat32 data 1
                let y = readFloat32 data 5
                Some(MouseMove(Vec2.Create(x, y)))

        | 0x02uy ->   // MouseDown
            if data.Length < 10 then Option.None
            else
                let x = readFloat32 data 1
                let y = readFloat32 data 5
                let b = int data.[9]
                Some(MouseDown(Vec2.Create(x, y), b))

        | 0x03uy ->   // MouseUp
            if data.Length < 10 then Option.None
            else
                let x = readFloat32 data 1
                let y = readFloat32 data 5
                let b = int data.[9]
                Some(MouseUp(Vec2.Create(x, y), b))

        | 0x04uy ->   // KeyDown
            if data.Length < 3 then Option.None
            else
                let len = int (readUInt16 data 1)
                if data.Length < 3 + len then Option.None
                else
                    let key = System.Text.Encoding.UTF8.GetString(data, 3, len)
                    Some(KeyDown key)

        | 0x05uy ->   // KeyUp
            if data.Length < 3 then Option.None
            else
                let len = int (readUInt16 data 1)
                if data.Length < 3 + len then Option.None
                else
                    let key = System.Text.Encoding.UTF8.GetString(data, 3, len)
                    Some(KeyUp key)

        | 0x06uy ->   // TextInput
            if data.Length < 3 then Option.None
            else
                let len = int (readUInt16 data 1)
                if data.Length < 3 + len then Option.None
                else
                    let text = System.Text.Encoding.UTF8.GetString(data, 3, len)
                    Some(TextInput text)

        | 0x07uy ->   // Wheel
            if data.Length < 5 then Option.None
            else
                let delta = readFloat32 data 1
                Some(Wheel delta)

        | _ -> Option.None

// ─────────────────────────────────────────────────────────────────────────────
// Runtime
// ─────────────────────────────────────────────────────────────────────────────

/// Mutable runtime state for a single TEA program instance.
type Runtime<'Model, 'Msg>(program: Program<'Model, 'Msg>) =

    // ── Mutable state ─────────────────────────────────────────────────────────
    let mutable model      : 'Model      = Unchecked.defaultof<'Model>
    let mutable prevScene  : Scene option = Option.None
    let mutable cachedTree : Render.Internal.CachedNode = Render.Internal.CachedEmpty
    let shapeCache                        = ShapeCache(256)

    // Pending message queue (FIFO).
    let msgQueue = System.Collections.Generic.Queue<'Msg>()

    // Command callbacks
    let mutable onSubmitFrame : (byte[] * int -> unit) option = Option.None

    // ── Initialisation ────────────────────────────────────────────────────────

    do
        let (initModel, initCmds) = program.Init()
        model <- initModel
        // Enqueue any immediate messages from startup commands.
        for cmd in Cmd.flatten initCmds do
            match cmd with
            | Emit msg -> msgQueue.Enqueue msg
            | _        -> ()

    // ── Dispatch ─────────────────────────────────────────────────────────────

    member _.Dispatch(msg: 'Msg) =
        msgQueue.Enqueue msg

    // ── Frame rendering ───────────────────────────────────────────────────────

    /// Called once per animation frame.  Processes all pending messages,
    /// diffs the scene, encodes the delta, and calls the submit callback.
    member self.RenderFrame(_timestamp: float) =
        // 1. Drain the message queue, accumulate commands.
        let mutable pendingCmds : Cmd<'Msg> list = []
        while msgQueue.Count > 0 do
            let msg = msgQueue.Dequeue()
            let (newModel, cmds) = program.Update model msg
            model <- newModel
            pendingCmds <- cmds :: pendingCmds

        // 2. View.
        let scene = program.View model

        // 3. Diff against previous scene.
        let delta = Diff.diffOpt prevScene scene

        // 4. Apply delta to cached tree.
        cachedTree <-
            match delta with
            | NoChange -> cachedTree
            | _        -> Render.Internal.CachedNode.applyDelta delta cachedTree

        // 5. Produce DrawList.
        let drawList = Render.Internal.CachedNode.getDrawList cachedTree shapeCache

        // 6. Encode into command buffer.
        let (buf, len) = Encode.encode drawList

        // 7. Submit.
        match onSubmitFrame with
        | Some f    -> f (buf, len)
        | Option.None -> ()

        prevScene <- Some scene

        // 8. Process side-effect commands.
        for cmd in List.collect Cmd.flatten pendingCmds do
            self.ExecuteCmd cmd

    // ── Input handling ────────────────────────────────────────────────────────

    member _.HandleInput(data: byte[]) =
        match InputDecoder.decode data with
        | Some evt ->
            // Walk the scene looking for Reactive nodes whose hit-test passes.
            let scene = program.View model
            Runtime<'Model,'Msg>.DispatchInput(evt, scene, msgQueue) |> ignore
        | Option.None -> ()

    // ── Command execution ─────────────────────────────────────────────────────

    member self.ExecuteCmd(cmd: Cmd<'Msg>) =
        match cmd with
        | Cmd.None    -> ()
        | Batch cmds  -> List.iter self.ExecuteCmd cmds
        | Emit msg    -> self.Dispatch msg
        | Timer(ms, msg) ->
            // In WASM the host drives timers; on desktop we use a simple
            // synchronous stub that fires immediately.
            ignore ms
            self.Dispatch msg
        | LoadImage _ | LoadFont _ ->
            // Host resolves these asynchronously; stub logs.
            ()

    // ── Configuration ────────────────────────────────────────────────────────

    member _.OnSubmitFrame(callback: byte[] * int -> unit) =
        onSubmitFrame <- Some callback

    member _.Model = model

    // ── Static factory helpers ────────────────────────────────────────────────

    /// Start a TEA program and return the Runtime handle.
    static member Start(program: Program<'Model, 'Msg>) =
        Runtime<'Model, 'Msg>(program)

    /// Convenience: run a program for exactly N frames with a list of
    /// per-frame message batches (useful for unit tests / headless rendering).
    static member RunHeadless
            (program : Program<'Model, 'Msg>)
            (frames  : 'Msg list list)
            : byte[] list =
        let rt = Runtime<'Model, 'Msg>(program)
        let results = System.Collections.Generic.List<byte[]>()
        rt.OnSubmitFrame(fun (buf, len) ->
            let copy = Array.sub buf 0 len
            results.Add copy)
        for frameMsgs in frames do
            for msg in frameMsgs do
                rt.Dispatch msg
            rt.RenderFrame(0.0)
        List.ofSeq results

    // ── Input dispatch helper ─────────────────────────────────────────────────

    static member private DispatchInput(evt: InputEvent, scene: Scene, queue: System.Collections.Generic.Queue<_>) =
        match scene with
        | Reactive(handler, child) ->
            let consumed = handler evt
            if not consumed then
                Runtime<'Model,'Msg>.DispatchInput(evt, child, queue) |> ignore
            consumed
        | Group children | Stack(_, _, _, children) ->
            children |> List.exists (fun c ->
                Runtime<'Model,'Msg>.DispatchInput(evt, c, queue))
        | Clip(_, c) | Padding(_, c) | Transform(_, c)
        | Opacity(_, c) | Animated(_, c) ->
            Runtime<'Model,'Msg>.DispatchInput(evt, c, queue)
        | RichText _ | Shape _ | Text _ | Image _ | Empty -> false

// ─────────────────────────────────────────────────────────────────────────────
// Module-level convenience helpers (renamed to avoid clash with type Runtime<_,_>)
// ─────────────────────────────────────────────────────────────────────────────

/// Functional helpers for creating and running a Runtime.
/// Prefer using the static members on Runtime<_,_> directly when the type is in scope.
module RuntimeHelpers =

    /// Start a TEA program and return the Runtime handle.
    let start (program: Program<'Model, 'Msg>) =
        Runtime<'Model, 'Msg>(program)

    /// Convenience: run a program for exactly N frames with a list of
    /// per-frame message batches (useful for unit tests / headless rendering).
    let runHeadless
            (program : Program<'Model, 'Msg>)
            (frames  : 'Msg list list)
            : byte[] list =
        let rt = start program
        let results = System.Collections.Generic.List<byte[]>()
        rt.OnSubmitFrame(fun (buf, len) ->
            let copy = Array.sub buf 0 len
            results.Add copy)
        for frameMsgs in frames do
            for msg in frameMsgs do
                rt.Dispatch msg
            rt.RenderFrame(0.0)
        List.ofSeq results
