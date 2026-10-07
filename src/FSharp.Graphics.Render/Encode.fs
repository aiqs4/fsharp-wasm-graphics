namespace FSharp.Graphics.Render

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives

/// Growable little-endian byte sink. Plain byte[] + int (no Span) so it compiles to WasmGC.
type ByteWriter(initialCapacity: int) =
    let mutable buf : byte[] = Array.zeroCreate (max 64 initialCapacity)
    let mutable pos = 0

    member _.Buffer = buf
    member _.Length = pos
    member _.Reset() = pos <- 0

    member _.Ensure(n: int) =
        if pos + n > buf.Length then
            let nb : byte[] = Array.zeroCreate (max (buf.Length * 2) (pos + n))
            Array.blit buf 0 nb 0 pos
            buf <- nb

    member w.U8(v: byte) =
        w.Ensure 1
        buf.[pos] <- v
        pos <- pos + 1

    member w.U32(v: uint32) =
        w.Ensure 4
        buf.[pos]     <- byte (v &&& 0xFFu)
        buf.[pos + 1] <- byte ((v >>> 8) &&& 0xFFu)
        buf.[pos + 2] <- byte ((v >>> 16) &&& 0xFFu)
        buf.[pos + 3] <- byte ((v >>> 24) &&& 0xFFu)
        pos <- pos + 4

    member w.U64(v: uint64) =
        w.U32(uint32 (v &&& 0xFFFFFFFFUL))
        w.U32(uint32 (v >>> 32))

    member w.F32(f: float32) = w.U32(uint32 (BitConverter.SingleToInt32Bits f))

module Encode =
    // ── Primitive writers ────────────────────────────────────────────────────

    let private color (w: ByteWriter) (c: Color) =
        w.U8 c.R; w.U8 c.G; w.U8 c.B; w.U8 c.A

    let private vec (w: ByteWriter) (v: Vec2) = w.F32 v.X; w.F32 v.Y

    let private rect (w: ByteWriter) (r: Rect) =
        w.F32 r.X; w.F32 r.Y; w.F32 r.W; w.F32 r.H

    let private stops (w: ByteWriter) (ss: GradientStop array) =
        w.U8 (byte (min 255 ss.Length))
        for s in ss do
            w.F32 s.Offset
            color w s.Color

    let paint (w: ByteWriter) (p: Paint) =
        match p with
        | Solid c ->
            w.U8 Protocol.PaintSolid
            color w c
        | LinearGradient(a, b, ss) ->
            w.U8 Protocol.PaintLinear
            vec w a; vec w b
            stops w ss
        | RadialGradient(c, r, ss) ->
            w.U8 Protocol.PaintRadial
            vec w c; w.F32 r
            stops w ss
        | ImageFill(ImageId id, opacity) ->
            w.U8 Protocol.PaintImage
            w.U32 id
            w.F32 opacity

    let geometry (w: ByteWriter) (g: ShapeGeometry) =
        match g with
        | GRect(b, r) ->
            w.U8 Protocol.GeomRect
            rect w b
            w.F32 r.TopLeft; w.F32 r.TopRight; w.F32 r.BottomRight; w.F32 r.BottomLeft
        | GCircle(c, r) ->
            w.U8 Protocol.GeomCircle
            vec w c; w.F32 r
        | GEllipse(c, rx, ry) ->
            w.U8 Protocol.GeomEllipse
            vec w c; w.F32 rx; w.F32 ry
        | GPolygon vs ->
            w.U8 Protocol.GeomPolygon
            w.U32 (uint32 vs.Length)
            for v in vs do vec w v
        | GLine(a, b, t) ->
            w.U8 Protocol.GeomLine
            vec w a; vec w b; w.F32 t
        | GOpaque b ->
            w.U8 Protocol.GeomOpaque
            rect w b

    let private blendIndex (b: Blend) : byte =
        match b with
        | Normal -> 0uy | Multiply -> 1uy | Screen -> 2uy | Overlay -> 3uy
        | Darken -> 4uy | Lighten -> 5uy | ColorDodge -> 6uy | ColorBurn -> 7uy
        | HardLight -> 8uy | SoftLight -> 9uy | Difference -> 10uy | Exclusion -> 11uy

    let private capIndex (c: LineCap) : byte =
        match c with Butt -> 0uy | Round -> 1uy | Square -> 2uy

    let private joinIndex (j: LineJoin) : byte =
        match j with Miter -> 0uy | RoundJoin -> 1uy | Bevel -> 2uy

    // ── Commands ─────────────────────────────────────────────────────────────

    let command (w: ByteWriter) (cmd: DrawCommand) : unit =
        let fill g p =
            w.U8 Protocol.OpFill
            geometry w g
            paint w p
        match cmd with
        | DrawRect(b, r, p)        -> fill (GRect(b, r)) p
        | DrawCircle(c, r, p)      -> fill (GCircle(c, r)) p
        | DrawEllipse(c, rx, ry, p)-> fill (GEllipse(c, rx, ry)) p
        | DrawPath(s, p)           -> fill s.Geometry p
        | DrawStroke(s, st) ->
            w.U8 Protocol.OpStroke
            geometry w s.Geometry
            w.F32 st.Width
            w.U8 (capIndex st.Cap)
            w.U8 (joinIndex st.Join)
            w.F32 st.MiterLimit
            paint w st.Paint
        | DrawShadow(s, sh) ->
            w.U8 Protocol.OpShadow
            geometry w s.Geometry
            vec w sh.Offset
            w.F32 sh.Blur
            w.F32 sh.Spread
            color w sh.Color
            w.U8 (if sh.Inner then 1uy else 0uy)
        | DrawTextBatch(FontId fid, size, glyphs, c) ->
            w.U8 Protocol.OpText
            w.U64 fid
            w.F32 size
            color w c
            w.U32 (uint32 glyphs.Length)
            for g in glyphs do
                w.U32 g.GlyphId; w.F32 g.X; w.F32 g.Y
        | DrawImage(ImageId id, b, opacity) ->
            w.U8 Protocol.OpImage
            w.U32 id
            rect w b
            w.F32 opacity
        | PushClip r -> w.U8 Protocol.OpPushClip; rect w r
        | PopClip    -> w.U8 Protocol.OpPopClip
        | PushTransform m ->
            w.U8 Protocol.OpPushTransform
            w.F32 m.M11; w.F32 m.M12; w.F32 m.M21; w.F32 m.M22; w.F32 m.M31; w.F32 m.M32
        | PopTransform -> w.U8 Protocol.OpPopTransform
        | PushLayer lp ->
            w.U8 Protocol.OpPushLayer
            w.F32 lp.Alpha
            w.F32 lp.BlurSigma
            w.U8 (blendIndex lp.Blend)
            w.U8 (byte lp.Filter.Length)
            for f in lp.Filter do w.F32 f
        | PopLayer -> w.U8 Protocol.OpPopLayer

    // ── Frames ───────────────────────────────────────────────────────────────

    /// Reusable encoder: keeps one growing buffer across frames (no per-frame allocation once warm).
    /// The returned buffer is only valid until the next call.
    type Encoder() =
        let w = ByteWriter(4096)
        let mutable seq = 0u
        member _.EncodeCommands(cmds: ResizeArray<DrawCommand>) : struct (byte[] * int) =
            w.Reset()
            w.U8 Protocol.OpBeginFrame
            w.U8 Protocol.Version
            w.U32 seq
            seq <- seq + 1u
            for i in 0 .. cmds.Count - 1 do command w cmds.[i]
            w.U8 Protocol.OpEndFrame
            struct (w.Buffer, w.Length)

    /// One-shot convenience (allocates). Returns (buffer, usedLength).
    let encode (drawList: DrawList) : byte[] * int =
        let w = ByteWriter(1024)
        w.U8 Protocol.OpBeginFrame
        w.U8 Protocol.Version
        w.U32 0u
        for c in drawList do command w c
        w.U8 Protocol.OpEndFrame
        (w.Buffer, w.Length)
