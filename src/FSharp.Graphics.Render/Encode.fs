/// Linear memory command-buffer encoder.
///
/// Serialises a DrawList into a compact binary format for zero-copy delivery
/// to the WASM host via a single `submit_frame(ptr, len)` call.
///
/// Opcode table (all values little-endian):
///   0xFE  BeginFrame    [opcode:1 | seq:4]
///   0x01  DrawRect      [opcode:1 | bounds:16 | corner_radius:16 | paint:variable]
///   0x02  DrawCircle    [opcode:1 | center:8 | radius:4 | paint:variable]
///   0x03  DrawEllipse   [opcode:1 | center:8 | rx:4 | ry:4 | paint:variable]
///   0x04  DrawPath      [opcode:1 | sdf_id:4 | paint:variable]
///   0x05  DrawTextBatch [opcode:1 | font_id:8 | size:4 | glyph_count:4 | glyphs:N*16 | color:4]
///   0x06  DrawImage     [opcode:1 | image_id:4 | dest_rect:16 | opacity:4]
///   0x10  PushClip      [opcode:1 | rect:16]
///   0x11  PopClip       [opcode:1]
///   0x12  PushTransform [opcode:1 | matrix:24]
///   0x13  PopTransform  [opcode:1]
///   0x14  SetOpacity    [opcode:1 | alpha:4]
///   0x20  DrawShadow    [opcode:1 | bounds:16 | blur:4 | spread:4 | offset:8 | color:4]
///   0x21  DrawStroke    [opcode:1 | width:4 | paint:variable]
///   0xFF  EndFrame      [opcode:1]
namespace FSharp.Graphics.Render

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open System

// ─────────────────────────────────────────────────────────────────────────────
// Encode module
// ─────────────────────────────────────────────────────────────────────────────

module Encode =

    // ── Opcodes ──────────────────────────────────────────────────────────────

    [<Literal>]
    let OpcodeBeginFrame    = 0xFEuy
    [<Literal>]
    let OpcodeEndFrame      = 0xFFuy
    [<Literal>]
    let OpcodeDrawRect      = 0x01uy
    [<Literal>]
    let OpcodeDrawCircle    = 0x02uy
    [<Literal>]
    let OpcodeDrawEllipse   = 0x03uy
    [<Literal>]
    let OpcodeDrawPath      = 0x04uy
    [<Literal>]
    let OpcodeDrawTextBatch = 0x05uy
    [<Literal>]
    let OpcodeDrawImage     = 0x06uy
    [<Literal>]
    let OpcodePushClip      = 0x10uy
    [<Literal>]
    let OpcodePopClip       = 0x11uy
    [<Literal>]
    let OpcodePushTransform = 0x12uy
    [<Literal>]
    let OpcodePopTransform  = 0x13uy
    [<Literal>]
    let OpcodeSetOpacity    = 0x14uy
    [<Literal>]
    let OpcodeDrawShadow    = 0x20uy
    [<Literal>]
    let OpcodeDrawStroke    = 0x21uy

    // ── Low-level writers ────────────────────────────────────────────────────

    let private writeByte (buf: byte[]) (offset: int) (v: byte) =
        buf.[offset] <- v
        offset + 1

    let private writeFloat32 (buf: byte[]) (offset: int) (v: float32) =
        let bits = BitConverter.SingleToInt32Bits v
        buf.[offset]     <- byte  (bits &&& 0xFF)
        buf.[offset + 1] <- byte ((bits >>> 8)  &&& 0xFF)
        buf.[offset + 2] <- byte ((bits >>> 16) &&& 0xFF)
        buf.[offset + 3] <- byte ((bits >>> 24) &&& 0xFF)
        offset + 4

    let private writeUInt32 (buf: byte[]) (offset: int) (v: uint32) =
        buf.[offset]     <- byte  (v &&& 0xFFu)
        buf.[offset + 1] <- byte ((v >>> 8)  &&& 0xFFu)
        buf.[offset + 2] <- byte ((v >>> 16) &&& 0xFFu)
        buf.[offset + 3] <- byte ((v >>> 24) &&& 0xFFu)
        offset + 4

    let private writeUInt64 (buf: byte[]) (offset: int) (v: uint64) =
        let lo = uint32 (v &&& 0xFFFFFFFFUL)
        let hi = uint32 (v >>> 32)
        let off = writeUInt32 buf offset lo
        writeUInt32 buf off hi

    let private writeRect (buf: byte[]) (offset: int) (r: Rect) =
        let off = writeFloat32 buf offset r.X
        let off = writeFloat32 buf off    r.Y
        let off = writeFloat32 buf off    r.W
        writeFloat32 buf off r.H

    let private writeVec2 (buf: byte[]) (offset: int) (v: Vec2) =
        let off = writeFloat32 buf offset v.X
        writeFloat32 buf off v.Y

    let private writeColor (buf: byte[]) (offset: int) (c: Color) =
        let off = writeByte buf offset c.R
        let off = writeByte buf off    c.G
        let off = writeByte buf off    c.B
        writeByte buf off c.A

    let private writeCornerRadius (buf: byte[]) (offset: int) (cr: CornerRadius) =
        let off = writeFloat32 buf offset cr.TopLeft
        let off = writeFloat32 buf off    cr.TopRight
        let off = writeFloat32 buf off    cr.BottomRight
        writeFloat32 buf off cr.BottomLeft

    let private writeMat3x2 (buf: byte[]) (offset: int) (m: Mat3x2) =
        let off = writeFloat32 buf offset m.M11
        let off = writeFloat32 buf off    m.M12
        let off = writeFloat32 buf off    m.M21
        let off = writeFloat32 buf off    m.M22
        let off = writeFloat32 buf off    m.M31
        writeFloat32 buf off m.M32

    // ── Paint encoding ───────────────────────────────────────────────────────

    [<Literal>]
    let private PaintSolid          = 0uy
    [<Literal>]
    let private PaintLinearGradient = 1uy
    [<Literal>]
    let private PaintRadialGradient = 2uy
    [<Literal>]
    let private PaintImage          = 3uy

    let private writePaint (buf: byte[]) (offset: int) (paint: Paint) =
        match paint with
        | Solid color ->
            let off = writeByte  buf offset PaintSolid
            writeColor buf off color

        | LinearGradient(stops, angleDeg) ->
            let off = writeByte   buf offset PaintLinearGradient
            let off = writeFloat32 buf off angleDeg
            let off = writeUInt32  buf off (uint32 stops.Length)
            stops |> Array.fold (fun o s ->
                let o = writeFloat32 buf o s.Offset
                writeColor buf o s.Color) off

        | RadialGradient(stops, center) ->
            let off = writeByte  buf offset PaintRadialGradient
            let off = writeVec2  buf off center
            let off = writeUInt32 buf off (uint32 stops.Length)
            stops |> Array.fold (fun o s ->
                let o = writeFloat32 buf o s.Offset
                writeColor buf o s.Color) off

        | ImageFill(ImageId id, opacity) ->
            let off = writeByte    buf offset PaintImage
            let off = writeUInt32  buf off id
            writeFloat32 buf off opacity

    // ── Buffer growth ────────────────────────────────────────────────────────

    let private ensureCapacity (buf: byte[] ref) (offset: int) (need: int) =
        let buf_ = buf.Value
        if offset + need > buf_.Length then
            let newLen = max (buf_.Length * 2) (offset + need + 1024)
            let newBuf = Array.zeroCreate newLen
            Array.blit buf_ 0 newBuf 0 offset
            buf.Value <- newBuf

    // ── Command encoder ──────────────────────────────────────────────────────

    let private writeCommand (buf: byte[] ref) (offset: int ref) (cmd: DrawCommand) =
        match cmd with
        | DrawRect(bounds, radius, paint) ->
            ensureCapacity buf offset.Value 512
            let off = writeByte  buf.Value offset.Value OpcodeDrawRect
            let off = writeRect  buf.Value off bounds
            let off = writeCornerRadius buf.Value off radius
            offset.Value <- writePaint buf.Value off paint

        | DrawCircle(center, radius, paint) ->
            ensureCapacity buf offset.Value 512
            let off = writeByte   buf.Value offset.Value OpcodeDrawCircle
            let off = writeVec2   buf.Value off center
            let off = writeFloat32 buf.Value off radius
            offset.Value <- writePaint buf.Value off paint

        | DrawEllipse(center, rx, ry, paint) ->
            ensureCapacity buf offset.Value 512
            let off = writeByte   buf.Value offset.Value OpcodeDrawEllipse
            let off = writeVec2   buf.Value off center
            let off = writeFloat32 buf.Value off rx
            let off = writeFloat32 buf.Value off ry
            offset.Value <- writePaint buf.Value off paint

        | DrawPath(sdfId, paint) ->
            ensureCapacity buf offset.Value 512
            let off = writeByte  buf.Value offset.Value OpcodeDrawPath
            let off = writeUInt32 buf.Value off (uint32 sdfId)
            offset.Value <- writePaint buf.Value off paint

        | DrawTextBatch(FontId fontId, sizePx, glyphs, color) ->
            ensureCapacity buf offset.Value (32 + glyphs.Length * 16)
            let off = writeByte    buf.Value offset.Value OpcodeDrawTextBatch
            let off = writeUInt64  buf.Value off fontId
            let off = writeFloat32 buf.Value off sizePx
            let off = writeUInt32  buf.Value off (uint32 glyphs.Length)
            let off =
                glyphs |> Array.fold (fun o g ->
                    let o = writeUInt32  buf.Value o g.GlyphId
                    let o = writeFloat32 buf.Value o g.X
                    let o = writeFloat32 buf.Value o g.Y
                    writeFloat32 buf.Value o g.AdvanceX) off
            offset.Value <- writeColor buf.Value off color

        | DrawImage(ImageId id, destRect, opacity) ->
            ensureCapacity buf offset.Value 32
            let off = writeByte    buf.Value offset.Value OpcodeDrawImage
            let off = writeUInt32  buf.Value off id
            let off = writeRect    buf.Value off destRect
            offset.Value <- writeFloat32 buf.Value off opacity

        | DrawStroke(_, stroke) ->
            ensureCapacity buf offset.Value 512
            let off = writeByte    buf.Value offset.Value OpcodeDrawStroke
            let off = writeFloat32 buf.Value off stroke.Width
            offset.Value <- writePaint buf.Value off stroke.Paint

        | DrawShadow(bounds, shadow) ->
            ensureCapacity buf offset.Value 64
            let off = writeByte    buf.Value offset.Value OpcodeDrawShadow
            let off = writeRect    buf.Value off bounds
            let off = writeFloat32 buf.Value off shadow.Blur
            let off = writeFloat32 buf.Value off shadow.Spread
            let off = writeVec2    buf.Value off shadow.Offset
            offset.Value <- writeColor buf.Value off shadow.Color

        | SetOpacity alpha ->
            ensureCapacity buf offset.Value 8
            let off = writeByte    buf.Value offset.Value OpcodeSetOpacity
            offset.Value <- writeFloat32 buf.Value off alpha

        | PushClip clipRect ->
            ensureCapacity buf offset.Value 20
            let off = writeByte buf.Value offset.Value OpcodePushClip
            offset.Value <- writeRect buf.Value off clipRect

        | PopClip ->
            ensureCapacity buf offset.Value 1
            offset.Value <- writeByte buf.Value offset.Value OpcodePopClip

        | PushTransform matrix ->
            ensureCapacity buf offset.Value 28
            let off = writeByte buf.Value offset.Value OpcodePushTransform
            offset.Value <- writeMat3x2 buf.Value off matrix

        | PopTransform ->
            ensureCapacity buf offset.Value 1
            offset.Value <- writeByte buf.Value offset.Value OpcodePopTransform

    // ── Public API ───────────────────────────────────────────────────────────

    /// Pre-allocated reusable command buffer (grows as needed).
    let mutable private commandBuffer : byte[] = Array.zeroCreate (256 * 1024)

    /// Frame sequence counter
    let mutable private frameSeq : uint32 = 0u

    /// Encode a DrawList into the shared command buffer.
    /// Returns the slice of the buffer containing the encoded frame.
    let encode (drawList: DrawList) : byte[] * int =
        frameSeq <- frameSeq + 1u
        let bufRef = ref commandBuffer
        let offRef = ref 0

        ensureCapacity bufRef 0 8
        offRef.Value <- writeByte bufRef.Value offRef.Value OpcodeBeginFrame
        offRef.Value <- writeUInt32 bufRef.Value offRef.Value frameSeq

        for cmd in drawList do
            writeCommand bufRef offRef cmd

        ensureCapacity bufRef offRef.Value 1
        offRef.Value <- writeByte bufRef.Value offRef.Value OpcodeEndFrame

        // Persist any grown buffer for next frame
        commandBuffer <- bufRef.Value
        (bufRef.Value, offRef.Value)

    /// Encode into a caller-supplied buffer. Returns the number of bytes
    /// written. The buffer must be large enough; use `encode` for automatic
    /// growth.
    let encodeInto (drawList: DrawList) (buffer: byte[]) : int =
        let bufRef = ref buffer
        let offRef = ref 0

        offRef.Value <- writeByte bufRef.Value offRef.Value OpcodeBeginFrame
        offRef.Value <- writeUInt32 bufRef.Value offRef.Value frameSeq

        for cmd in drawList do
            writeCommand bufRef offRef cmd

        offRef.Value <- writeByte bufRef.Value offRef.Value OpcodeEndFrame
        offRef.Value
