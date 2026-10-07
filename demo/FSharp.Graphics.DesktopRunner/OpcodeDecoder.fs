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

    // TODO: this is better if it's inlined
    let readColor (buffer: ReadOnlySpan<byte>) (offset: byref<int>) =
        let r = readByte buffer &offset
        let g = readByte buffer &offset
        let b = readByte buffer &offset
        let a = readByte buffer &offset
        SKColor(r, g, b, a)

    let readPaint (buffer: ReadOnlySpan<byte>) (offset: byref<int>) (paint: SKPaint) =
        let paintType = readByte buffer &offset
        match paintType with
        | 0uy -> // Solid
            let color = readColor buffer &offset
            paint.Shader <- null
            paint.Color <- color
        | 1uy -> // LinearGradient
            let _angleDeg = readSingle buffer &offset
            let count = readUInt32 buffer &offset
            let mutable firstColor = SKColors.Black
            for i = 0 to int count - 1 do
                let _stopOffset = readSingle buffer &offset
                let c = readColor buffer &offset
                if i = 0 then firstColor <- c
            paint.Shader <- null
            paint.Color <- firstColor
        | 2uy -> // RadialGradient
            let _cx = readSingle buffer &offset
            let _cy = readSingle buffer &offset
            let count = readUInt32 buffer &offset
            let mutable firstColor = SKColors.Black
            for i = 0 to int count - 1 do
                let _stopOffset = readSingle buffer &offset
                let c = readColor buffer &offset
                if i = 0 then firstColor <- c
            paint.Shader <- null
            paint.Color <- firstColor
        | 3uy -> // Image
            let _imageId = readUInt32 buffer &offset
            let _opacity = readSingle buffer &offset
            paint.Shader <- null
            paint.Color <- SKColors.Gray
        | _ -> ()

    /// Decodes a flat binary command buffer emitted by FSharp.Graphics.Render.Encode.
    member this.DecodeAndRender(buffer: ReadOnlySpan<byte>) =
        let mutable offset = 0
        let mutable executing = true
        
        while executing && offset < buffer.Length do
            match readByte buffer &offset with
            
            // 0xFE: BeginFrame [seq: uint32]
            | 0xFEuy ->
                // let r = readByte buffer &offset
                // let g = readByte buffer &offset
                // let b = readByte buffer &offset
                // let a = readByte buffer &offset
                // canvas.Clear(SKColor(r, g, b, a))
                let _seq = readUInt32 buffer &offset
                canvas.Clear(SKColors.White)

            // 0x01: DrawRect [bounds: 16b | corner_radius: 16b | paint: var]
            | 0x01uy ->
                let x = readSingle buffer &offset
                let y = readSingle buffer &offset
                let w = readSingle buffer &offset
                let h = readSingle buffer &offset
                let tl = readSingle buffer &offset
                let tr = readSingle buffer &offset
                let br = readSingle buffer &offset
                let bl = readSingle buffer &offset
                
                readPaint buffer &offset fillPaint
                let rect = SKRect(x, y, x + w, y + h)
                let radius = max (max tl tr) (max br bl)

                if radius > 0.0f then
                    canvas.DrawRoundRect(rect, radius, radius, fillPaint)
                else
                    canvas.DrawRect(rect, fillPaint)

            // 0x02: DrawCircle [center: 8b | radius: 4b | paint: var]
            | 0x02uy ->
                let cx = readSingle buffer &offset
                let cy = readSingle buffer &offset
                let r = readSingle buffer &offset
                readPaint buffer &offset fillPaint
                canvas.DrawCircle(cx, cy, r, fillPaint)

            // 0x03: DrawEllipse [center: 8b | rx: 4b | ry: 4b | paint: var]
            | 0x03uy ->
                let cx = readSingle buffer &offset
                let cy = readSingle buffer &offset
                let rx = readSingle buffer &offset
                let ry = readSingle buffer &offset
                readPaint buffer &offset fillPaint
                canvas.DrawOval(SKRect(cx - rx, cy - ry, cx + rx, cy + ry), fillPaint)

            // 0x04: DrawPath [sdf_id: 4b | paint: var]
            | 0x04uy ->
                let _sdfId = readUInt32 buffer &offset
                readPaint buffer &offset fillPaint

            // 0x05: DrawTextBatch [font_id: 8b | size: 4b | count: 4b | glyphs: N*16b | color: 4b]
            | 0x05uy ->
                let _fontId = readUInt64 buffer &offset
                let sizePx = readSingle buffer &offset
                let count = readUInt32 buffer &offset
                
                let glyphs = Array.zeroCreate<uint16> (int count)
                let positions = Array.zeroCreate<SKPoint> (int count)
                
                for i = 0 to int count - 1 do
                    let glyphId = readUInt32 buffer &offset
                    let gx = readSingle buffer &offset
                    let gy = readSingle buffer &offset
                    let _advX = readSingle buffer &offset
                    glyphs.[i] <- uint16 glyphId
                    positions.[i] <- SKPoint(gx, gy)
                
                let color = readColor buffer &offset
                textPaint.Color <- color
                use font = new SKFont(typeface, sizePx)
                
                if count > 0u then
                    use builder = new SKTextBlobBuilder()
                    let run = builder.AllocatePositionedRun(font, int count)
                    let glyphSpan = run.GetGlyphSpan()
                    let posSpan = run.GetPositionSpan()
                    
                    for i = 0 to int count - 1 do
                        glyphSpan[i] <- glyphs.[i]
                        posSpan[i] <- positions.[i]
                    
                    use blob = builder.Build()
                    canvas.DrawText(blob, 0f, 0f, textPaint)

            // 0x06: DrawImage [image_id: 4b | dest_rect: 16b | opacity: 4b]
            // TODO: not yet implemented
            // | 0x06uy ->
            //     let _imageId = readUInt32 buffer &offset
            //     let _x = readSingle buffer &offset
            //     let _y = readSingle buffer &offset
            //     let _w = readSingle buffer &offset
            //     let _h = readSingle buffer &offset
            //     let _opacity = readSingle buffer &offset

            // 0x10: PushClip [rect: 16b]
            | 0x10uy ->
                let x = readSingle buffer &offset
                let y = readSingle buffer &offset
                let w = readSingle buffer &offset
                let h = readSingle buffer &offset
                canvas.Save() |> ignore
                canvas.ClipRect(SKRect(x, y, x + w, y + h), SKClipOperation.Intersect, true)

            // 0x11: PopClip
            | 0x11uy ->
                canvas.Restore()

            // 0x12: PushTransform (3x2 Matrix) [matrix: 24b]
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

            // 0x14: SetOpacity [alpha: 4b]
            | 0x14uy ->
                let alpha = readSingle buffer &offset
                fillPaint.Color <- fillPaint.Color.WithAlpha(byte (alpha * 255.0f))

            // 0x20: DrawShadow [bounds: 16b | blur: 4b | spread: 4b | offset: 8b | color: 4b]
            // TODO: not yet implemented
            // | 0x20uy ->
            //     let _x = readSingle buffer &offset
            //     let _y = readSingle buffer &offset
            //     let _w = readSingle buffer &offset
            //     let _h = readSingle buffer &offset
            //     let _blur = readSingle buffer &offset
            //     let _spread = readSingle buffer &offset
            //     let _ox = readSingle buffer &offset
            //     let _oy = readSingle buffer &offset
            //     let _color = readColor buffer &offset

            // 0x21: DrawStroke [width: 4b | paint: var]
            | 0x21uy ->
                let _width = readSingle buffer &offset
                readPaint buffer &offset fillPaint

            // 0xFF: EndFrame
            | 0xFFuy ->
                executing <- false

            | unknown ->
                printfn "Warning: Encountered unknown opcode 0x%X" unknown
                executing <- false
