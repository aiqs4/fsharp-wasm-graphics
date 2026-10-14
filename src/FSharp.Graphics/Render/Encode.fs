/// Linear memory command-buffer encoder.
///
/// Serialises a DrawList into a compact binary format for zero-copy delivery
/// to the WASM host via a single `submit_frame(ptr, len)` call.
///
/// Opcode table (all values little-endian):
///   0xFE  BeginFrame    [opcode:1]
///   0x01  DrawShape     [opcode:1 | bounds:16 | paint_type:1 | color_or_stop_count:16 | stroke:5 | shadow:17]
///   0x02  DrawTextBatch [opcode:1 | font_id:8 | size:4 | color:16 | glyph_count:4 | glyphs:N*16]
///   0x03  DrawImage     [opcode:1 | image_id:8 | dest_rect:16 | opacity:4]
///   0x04  PushClip      [opcode:1 | rect:16]
///   0x05  PopClip       [opcode:1]
///   0x06  SetTransform  [opcode:1 | matrix:24]
///   0x07  SetOpacity    [opcode:1 | alpha:4]
///   0xFF  EndFrame      [opcode:1]
namespace FSharp.Graphics.Render

open FSharp.Graphics
open System

// ─────────────────────────────────────────────────────────────────────────────
// Encode module
// ─────────────────────────────────────────────────────────────────────────────

module Encode =

    // ── Opcodes ──────────────────────────────────────────────────────────────

    [<Literal>]
    let OpcodeBeginFrame   = 0xFEuy
    [<Literal>]
    let OpcodeDrawShape    = 0x01uy
    [<Literal>]
    let OpcodeDrawText     = 0x02uy
    [<Literal>]
    let OpcodeDrawImage    = 0x03uy
    [<Literal>]
    let OpcodePushClip     = 0x04uy
    [<Literal>]
    let OpcodePopClip      = 0x05uy
    [<Literal>]
    let OpcodeSetTransform = 0x06uy
    [<Literal>]
    let OpcodeSetOpacity   = 0x07uy
    [<Literal>]
    let OpcodeEndFrame     = 0xFFuy

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
        let off = writeFloat32 buf offset   r.X
        let off = writeFloat32 buf off      r.Y
        let off = writeFloat32 buf off      r.W
        writeFloat32 buf off r.H

    let private writeVec2 (buf: byte[]) (offset: int) (v: Vec2) =
        let off = writeFloat32 buf offset v.X
        writeFloat32 buf off v.Y

    let private writeColor (buf: byte[]) (offset: int) (c: Color) =
        let off = writeFloat32 buf offset c.R
        let off = writeFloat32 buf off    c.G
        let off = writeFloat32 buf off    c.B
        writeFloat32 buf off c.A

    // ── Paint encoding ───────────────────────────────────────────────────────

    // Paint type tags
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

        | LinearGradient(from', to', stops) ->
            let off = writeByte  buf offset PaintLinearGradient
            let off = writeVec2  buf off from'
            let off = writeVec2  buf off to'
            let off = writeUInt32 buf off (uint32 stops.Length)
            stops |> Array.fold (fun o s ->
                let o = writeFloat32 buf o   s.Offset
                writeColor   buf o s.Color) off

        | RadialGradient(center, radius, stops) ->
            let off = writeByte   buf offset PaintRadialGradient
            let off = writeVec2   buf off center
            let off = writeFloat32 buf off radius
            let off = writeUInt32  buf off (uint32 stops.Length)
            stops |> Array.fold (fun o s ->
                let o = writeFloat32 buf o   s.Offset
                writeColor   buf o s.Color) off

        | ImagePaint(ImageId id, srcRect, opacity) ->
            let off = writeByte    buf offset PaintImage
            let off = writeUInt64  buf off id
            let off = writeRect    buf off srcRect
            writeFloat32 buf off opacity

    // ── Stroke / Shadow encoding ─────────────────────────────────────────────

    let private writeStrokeOpt (buf: byte[]) (offset: int) (stroke: Stroke option) =
        match stroke with
        | None ->
            writeByte buf offset 0uy
        | Some s ->
            let off = writeByte    buf offset 1uy
            let off = writePaint   buf off s.Paint
            writeFloat32 buf off s.Thickness

    let private writeShadowOpt (buf: byte[]) (offset: int) (shadow: Shadow option) =
        match shadow with
        | None ->
            writeByte buf offset 0uy
        | Some sh ->
            let off = writeByte    buf offset 1uy
            let off = writeVec2    buf off sh.Offset
            let off = writeFloat32 buf off sh.Blur
            let off = writeFloat32 buf off sh.Spread
            writeColor buf off sh.Color

    // ── Command encoder ──────────────────────────────────────────────────────

    let private ensureCapacity (buf: byte[] ref) (offset: int) (need: int) =
        let buf_ = !buf
        if offset + need > buf_.Length then
            let newLen = max (buf_.Length * 2) (offset + need + 1024)
            let newBuf = Array.zeroCreate newLen
            Array.blit buf_ 0 newBuf 0 offset
            buf := newBuf

    let private writeCommand (buf: byte[] ref) (offset: int ref) (cmd: DrawCommand) =
        match cmd with
        | BeginFrame ->
            ensureCapacity buf !offset 1
            offset := writeByte !buf !offset OpcodeBeginFrame

        | EndFrame ->
            ensureCapacity buf !offset 1
            offset := writeByte !buf !offset OpcodeEndFrame

        | DrawShape(shape, paint, stroke, shadow) ->
            let bounds  = shape.Bounds
            // Conservative size estimate; ensureCapacity handles overflows.
            ensureCapacity buf !offset 512
            let off = writeByte  !buf !offset OpcodeDrawShape
            let off = writeRect  !buf off bounds
            let off = writePaint !buf off paint
            let off = writeStrokeOpt !buf off stroke
            let off = writeShadowOpt !buf off shadow
            offset := off

        | DrawTextBatch(glyphs, fontId, size, color) ->
            ensureCapacity buf !offset (32 + glyphs.Length * 16)
            let off = writeByte   !buf !offset OpcodeDrawText
            let off = writeUInt64 !buf off fontId
            let off = writeFloat32 !buf off size
            let off = writeColor  !buf off color
            let off = writeUInt32 !buf off (uint32 glyphs.Length)
            let off =
                glyphs |> Array.fold (fun o g ->
                    let o = writeUInt32  !buf o g.GlyphId
                    let o = writeFloat32 !buf o g.X
                    let o = writeFloat32 !buf o g.Y
                    writeFloat32 !buf o g.AdvanceX) off
            offset := off

        | DrawImage(ImageId id, destRect, opacity) ->
            ensureCapacity buf !offset 32
            let off = writeByte    !buf !offset OpcodeDrawImage
            let off = writeUInt64  !buf off id
            let off = writeRect    !buf off destRect
            offset := writeFloat32 !buf off opacity

        | PushClip clipRect ->
            ensureCapacity buf !offset 20
            let off = writeByte !buf !offset OpcodePushClip
            offset := writeRect !buf off clipRect

        | PopClip ->
            ensureCapacity buf !offset 1
            offset := writeByte !buf !offset OpcodePopClip

        | SetTransform matrix ->
            ensureCapacity buf !offset 28
            let off = writeByte   !buf !offset OpcodeSetTransform
            let off = writeFloat32 !buf off matrix.M11
            let off = writeFloat32 !buf off matrix.M12
            let off = writeFloat32 !buf off matrix.M21
            let off = writeFloat32 !buf off matrix.M22
            let off = writeFloat32 !buf off matrix.M31
            offset := writeFloat32 !buf off matrix.M32

        | SetOpacity alpha ->
            ensureCapacity buf !offset 5
            let off = writeByte    !buf !offset OpcodeSetOpacity
            offset := writeFloat32 !buf off alpha

    // ── Public API ───────────────────────────────────────────────────────────

    /// Pre-allocated reusable command buffer (grows as needed).
    let mutable private commandBuffer : byte[] = Array.zeroCreate 65536

    /// Encode a DrawList into the shared command buffer.
    /// Returns the slice of the buffer containing the encoded frame.
    let encode (drawList: DrawList) : byte[] * int =
        let bufRef = ref commandBuffer
        let offRef = ref 0

        writeCommand bufRef offRef BeginFrame
        for cmd in drawList do
            writeCommand bufRef offRef cmd
        writeCommand bufRef offRef EndFrame

        // Persist any grown buffer for next frame.
        commandBuffer <- !bufRef
        (!bufRef, !offRef)

    /// Encode into a caller-supplied buffer.  Returns the number of bytes
    /// written.  The buffer must be large enough; use `encode` for automatic
    /// growth.
    let encodeInto (drawList: DrawList) (buffer: byte[]) : int =
        let bufRef = ref buffer
        let offRef = ref 0
        writeCommand bufRef offRef BeginFrame
        for cmd in drawList do
            writeCommand bufRef offRef cmd
        writeCommand bufRef offRef EndFrame
        !offRef
