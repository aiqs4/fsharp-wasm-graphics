namespace FSharp.Graphics.Render

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

module Pipeline =

    /// One-shot: resolve -> cache -> draw list -> bytes. Allocates; for tests and static export.
    /// Prefer `Stage` for anything that runs every frame.
    let encodeSceneWith (te: ITextEngine) (viewportW: float32) (scene: Scene) : byte[] * int =
        let resolved = Resolve.resolve te viewportW scene
        let cached   = CachedNode.build resolved
        Encode.encode (CachedNode.getDrawList cached (ShapeCache(1)))

    /// Back-compat entry point (no text engine: Text nodes draw nothing).
    let encodeScene (scene: Scene) (shapeCache: ShapeCache) : byte[] * int =
        let cached = CachedNode.build (Resolve.resolve TextEngine.approximate System.Single.MaxValue scene)
        Encode.encode (CachedNode.getDrawList cached shapeCache)

/// The retained "stage" (cf. ClutterStage): owns the text engine, the last resolved scene,
/// the pointer-routing state and a reusable encoder. One call per frame:
///
///     let struct (buf, len) = stage.Frame scene       // bytes for any backend
///     stage.Input (PointerMove p) |> ignore           // routed through the last frame
type Stage(text: ITextEngine, width: float32, height: float32) =
    let encoder    = Encode.Encoder()
    let dispatcher = Hit.Dispatcher()
    let commands   = ResizeArray<DrawCommand>(1024)
    let mutable resolved = Scene.Empty
    let mutable w = width
    let mutable h = height

    member _.Size = Vec2.create w h
    member _.Resize(nw: float32, nh: float32) = w <- nw; h <- nh
    /// The scene as drawn last frame (layout applied, text shaped).
    member _.Resolved = resolved
    member _.Hovered = dispatcher.Hover

    /// Resolve layout/text, build the command list, encode. Buffer valid until the next call.
    member _.Frame(scene: Scene) : struct (byte[] * int) =
        resolved <- Resolve.resolve text w scene
        commands.Clear()
        CachedNode.emit (CachedNode.build resolved) commands
        encoder.EncodeCommands commands

    /// Route an input event through the last frame's scene.
    member _.Input(evt: InputEvent) : bool = dispatcher.Handle(resolved, evt)
