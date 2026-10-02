module FSharp.Graphics.DesktopRunner.OpcodeDecoder

open System
open System.Buffers.Binary
open SkiaSharp

[<AutoOpen>]
module BinaryHelpers =
    let inline readByte (buffer: ReadOnlySpan<byte>) (offset: byref<int>) =
        let v = buffer[offset]
        offset <- offset + 1
        v

    let inline readSingle (buffer: ReadOnlySpan<byte>) (offset: byref<int>) =
        let v = BinaryPrimitives.ReadSingleLittleEndian(buffer.Slice(offset))
        offset <- offset + 4
        v

    let inline readInt32 (buffer: ReadOnlySpan<byte>) (offset: byref<int>) =
        let v = BinaryPrimitives.ReadInt32LittleEndian(buffer.Slice(offset))
        offset <- offset + 4
        v

    let inline readUInt32 (buffer: ReadOnlySpan<byte>) (offset: byref<int>) =
        let v = BinaryPrimitives.ReadUInt32LittleEndian(buffer.Slice(offset))
        offset <- offset + 4
        v

    let inline readUInt64 (buffer: ReadOnlySpan<byte>) (offset: byref<int>) =
        let v = BinaryPrimitives.ReadUInt64LittleEndian(buffer.Slice(offset))
        offset <- offset + 8
        v


/// Zero-allocation binary opcode parser converting memory byte spans into SkiaSharp canvas calls.
type SkiaOpcodeDecoder(canvas: SKCanvas, typeface: SKTypeface) =

    let fillPaint = new SKPaint(IsAntialias = true, Style = SKPaintStyle.Fill)
    let textPaint = new SKPaint(IsAntialias = true, Style = SKPaintStyle.Fill)

    /// Decodes a flat binary command buffer emitted by FSharp.Graphics.Render.Encode.
    member this.DecodeAndRender(buffer: ReadOnlySpan<byte>) =
        let mutable offset = 0
        let mutable executing = true
        
        while executing && offset < buffer.Length do
            match readByte buffer &offset with
            
            // 0xFE: BeginFrame
            | 0xFEuy ->
                let r = readByte buffer &offset
                let g = readByte buffer &offset
                let b = readByte buffer &offset
                let a = readByte buffer &offset
                canvas.Clear(SKColor(r, g, b, a))

            // 0x01: DrawRect / DrawShape
            | 0x01uy ->
                let x = readSingle buffer &offset
                let y = readSingle buffer &offset
                let w = readSingle buffer &offset
                let h = readSingle buffer &offset
                let radius = readSingle buffer &offset
                
                let r = readByte buffer &offset
                let g = readByte buffer &offset
                let b = readByte buffer &offset
                let a = readByte buffer &offset

                fillPaint.Color <- SKColor(r, g, b, a)
                let rect = SKRect(x, y, x + w, y + h)

                if radius > 0.0f then
                    canvas.DrawRoundRect(rect, radius, radius, fillPaint)
                else
                    canvas.DrawRect(rect, fillPaint)

            // 0x02: DrawTextBatch
            | 0x02uy ->
                let _fontId = readUInt64 buffer &offset
                let sizePx = readSingle buffer &offset
                
                let r = readByte buffer &offset
                let g = readByte buffer &offset
                let b = readByte buffer &offset
                let a = readByte buffer &offset
                
                textPaint.Color <- SKColor(r, g, b, a)
                
                use font = new SKFont(typeface, sizePx)
                let count = readInt32 buffer &offset
                
                if count > 0 then
                    use builder = new SKTextBlobBuilder()
                    let run = builder.AllocatePositionedRun(font, count)
                    let glyphSpan = run.GetGlyphSpan()
                    let posSpan = run.GetPositionSpan()
                    
                    for i = 0 to count - 1 do
                        let glyphId = readUInt32 buffer &offset
                        let gx = readSingle buffer &offset
                        let gy = readSingle buffer &offset
                        let _advX = readSingle buffer &offset 
                        
                        glyphSpan[i] <- uint16 glyphId
                        posSpan[i] <- SKPoint(gx, gy)
                    
                    use blob = builder.Build()
                    canvas.DrawText(blob, 0f, 0f, textPaint)

            // 0x04: PushClip
            | 0x04uy ->
                let x = readSingle buffer &offset
                let y = readSingle buffer &offset
                let w = readSingle buffer &offset
                let h = readSingle buffer &offset
                canvas.Save() |> ignore
                canvas.ClipRect(SKRect(x, y, x + w, y + h), SKClipOperation.Intersect, true)

            // 0x05: PopClip
            | 0x05uy ->
                canvas.Restore()

            // 0x10: SetOpacity
            | 0x10uy ->
                let alpha = readSingle buffer &offset
                fillPaint.Color <- fillPaint.Color.WithAlpha(byte (alpha * 255.0f))

            // 0x12: PushTransform (3x2 Matrix)
            | 0x12uy ->
                let m11 = readSingle buffer &offset
                let m12 = readSingle buffer &offset
                let m21 = readSingle buffer &offset
                let m22 = readSingle buffer &offset
                let m31 = readSingle buffer &offset
                let m32 = readSingle buffer &offset

                canvas.Save() |> ignore
                let mutable matrix = SKMatrix.CreateIdentity()
                matrix.ScaleX <- m11
                matrix.SkewY <- m12
                matrix.SkewX <- m21
                matrix.ScaleY <- m22
                matrix.TransX <- m31
                matrix.TransY <- m32
                canvas.Concat(&matrix)

            // 0x13: PopTransform
            | 0x13uy ->
                canvas.Restore()

            // 0xFF: EndFrame
            | 0xFFuy ->
                executing <- false

            | unknown ->
                printfn "Warning: Encountered unknown opcode 0x%X" unknown
                executing <- false