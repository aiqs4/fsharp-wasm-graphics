namespace FSharp.Graphics.Host

open System
open System.Buffers.Binary
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Render

/// What a rendering backend must implement. The decoder turns bytes into these calls;
/// the software rasterizer, Skia, (later) Canvas2D / WebGPU are all just implementations.
type IRenderSink =
    abstract BeginFrame    : version: byte * seq: uint32 -> unit
    abstract EndFrame      : unit -> unit
    abstract Fill          : geom: ShapeGeometry * paint: Paint -> unit
    abstract Stroke        : geom: ShapeGeometry * stroke: Stroke -> unit
    abstract Shadow        : geom: ShapeGeometry * shadow: Shadow -> unit
    abstract Text          : fontId: FontId * sizePx: float32 * color: Color * glyphs: GlyphPosition array -> unit
    abstract Image         : id: ImageId * bounds: Rect * opacity: float32 -> unit
    abstract PushClip      : Rect -> unit
    abstract PopClip       : unit -> unit
    abstract PushTransform : Mat3x2 -> unit
    abstract PopTransform  : unit -> unit
    abstract PushLayer     : LayerParams -> unit
    abstract PopLayer      : unit -> unit

type DecodeResult =
    /// Whole frame decoded; payload = number of drawing/state commands.
    | Decoded   of commands: int
    /// Stream is corrupt. Decoding stops at the offending byte instead of guessing.
    | Malformed of offset: int * message: string

exception private DecodeFailure of offset: int * message: string

/// Bounds-checked little-endian cursor over the encoder's buffer.
type private Cursor(buf: byte[], len: int) =
    let mutable pos = 0
    member _.Pos = pos
    member _.AtEnd = pos >= len
    member private _.Need(n: int) =
        if pos + n > len then raise (DecodeFailure(pos, "unexpected end of stream"))
    member c.U8() : byte =
        c.Need 1
        let v = buf.[pos]
        pos <- pos + 1
        v
    member c.U32() : uint32 =
        c.Need 4
        let v = BinaryPrimitives.ReadUInt32LittleEndian(ReadOnlySpan<byte>(buf, pos, 4))
        pos <- pos + 4
        v
    member c.U64() : uint64 =
        c.Need 8
        let v = BinaryPrimitives.ReadUInt64LittleEndian(ReadOnlySpan<byte>(buf, pos, 8))
        pos <- pos + 8
        v
    member c.F32() : float32 = BitConverter.UInt32BitsToSingle(c.U32())
    member c.Vec() : Vec2 = let x = c.F32() in let y = c.F32() in Vec2.create x y
    member c.Rect() : Rect =
        let x = c.F32() in let y = c.F32() in let w = c.F32() in let h = c.F32()
        Rect.create x y w h
    member c.Color() : Color =
        let r = c.U8() in let g = c.U8() in let b = c.U8() in let a = c.U8()
        Color.rgba r g b a

module CommandReader =

    let private stops (c: Cursor) : GradientStop array =
        let n = int (c.U8())
        Array.init n (fun _ ->
            let o = c.F32()
            let col = c.Color()
            { Offset = o; Color = col })

    let private paint (c: Cursor) : Paint =
        match c.U8() with
        | Protocol.PaintSolid  -> Solid (c.Color())
        | Protocol.PaintLinear ->
            let a = c.Vec() in let b = c.Vec()
            LinearGradient(a, b, stops c)
        | Protocol.PaintRadial ->
            let ctr = c.Vec() in let r = c.F32()
            RadialGradient(ctr, r, stops c)
        | Protocol.PaintImage  ->
            let id = c.U32() in let op = c.F32()
            ImageFill(ImageId id, op)
        | t -> raise (DecodeFailure(c.Pos, sprintf "unknown paint tag %d" t))

    let private geometry (c: Cursor) : ShapeGeometry =
        match c.U8() with
        | Protocol.GeomRect ->
            let b = c.Rect()
            let tl = c.F32() in let tr = c.F32() in let br = c.F32() in let bl = c.F32()
            GRect(b, { TopLeft = tl; TopRight = tr; BottomRight = br; BottomLeft = bl })
        | Protocol.GeomCircle  -> let ctr = c.Vec() in GCircle(ctr, c.F32())
        | Protocol.GeomEllipse -> let ctr = c.Vec() in let rx = c.F32() in GEllipse(ctr, rx, c.F32())
        | Protocol.GeomPolygon ->
            let n = int (c.U32())
            GPolygon(Array.init n (fun _ -> c.Vec()))
        | Protocol.GeomLine ->
            let a = c.Vec() in let b = c.Vec()
            GLine(a, b, c.F32())
        | Protocol.GeomOpaque  -> GOpaque(c.Rect())
        | t -> raise (DecodeFailure(c.Pos, sprintf "unknown geometry tag %d" t))

    let private blend (i: byte) : Blend =
        match i with
        | 1uy -> Multiply | 2uy -> Screen | 3uy -> Overlay | 4uy -> Darken | 5uy -> Lighten
        | 6uy -> ColorDodge | 7uy -> ColorBurn | 8uy -> HardLight | 9uy -> SoftLight
        | 10uy -> Difference | 11uy -> Exclusion | _ -> Normal

    /// Decode one frame and replay it onto `sink`. Never throws on bad input.
    let run (sink: IRenderSink) (buf: byte[]) (len: int) : DecodeResult =
        let c = Cursor(buf, len)
        let mutable count = 0
        try
            if c.U8() <> Protocol.OpBeginFrame then raise (DecodeFailure(0, "stream must start with BeginFrame"))
            let version = c.U8()
            if version <> Protocol.Version then
                raise (DecodeFailure(1, sprintf "protocol version %d, decoder speaks %d" version Protocol.Version))
            sink.BeginFrame(version, c.U32())
            let mutable fin = false
            while not fin do
                let at = c.Pos
                match c.U8() with
                | Protocol.OpEndFrame -> fin <- true
                | Protocol.OpFill ->
                    let g = geometry c in sink.Fill(g, paint c); count <- count + 1
                | Protocol.OpStroke ->
                    let g = geometry c
                    let w = c.F32()
                    let cap = match c.U8() with 1uy -> Round | 2uy -> Square | _ -> Butt
                    let join = match c.U8() with 1uy -> RoundJoin | 2uy -> Bevel | _ -> Miter
                    let miter = c.F32()
                    let p = paint c
                    sink.Stroke(g, { Paint = p; Width = w; Cap = cap; Join = join; MiterLimit = miter })
                    count <- count + 1
                | Protocol.OpShadow ->
                    let g = geometry c
                    let off = c.Vec()
                    let blur = c.F32() in let spread = c.F32()
                    let col = c.Color()
                    let inner = c.U8() <> 0uy
                    sink.Shadow(g, { Offset = off; Blur = blur; Spread = spread; Color = col; Inner = inner })
                    count <- count + 1
                | Protocol.OpText ->
                    let fid = c.U64() in let size = c.F32() in let col = c.Color()
                    let n = int (c.U32())
                    let glyphs =
                        Array.init n (fun _ ->
                            let gid = c.U32() in let x = c.F32() in let y = c.F32()
                            { GlyphId = gid; X = x; Y = y; AdvanceX = 0.0f })
                    sink.Text(FontId fid, size, col, glyphs)
                    count <- count + 1
                | Protocol.OpImage ->
                    let id = c.U32() in let r = c.Rect()
                    sink.Image(ImageId id, r, c.F32())
                    count <- count + 1
                | Protocol.OpPushClip      -> sink.PushClip(c.Rect()); count <- count + 1
                | Protocol.OpPopClip       -> sink.PopClip(); count <- count + 1
                | Protocol.OpPushTransform ->
                    let m11 = c.F32() in let m12 = c.F32() in let m21 = c.F32()
                    let m22 = c.F32() in let m31 = c.F32() in let m32 = c.F32()
                    sink.PushTransform { M11 = m11; M12 = m12; M21 = m21; M22 = m22; M31 = m31; M32 = m32 }
                    count <- count + 1
                | Protocol.OpPopTransform  -> sink.PopTransform(); count <- count + 1
                | Protocol.OpPushLayer ->
                    let alpha = c.F32() in let sigma = c.F32()
                    let bl = blend (c.U8())
                    let n = int (c.U8())
                    let filter = Array.init n (fun _ -> c.F32())
                    sink.PushLayer { Alpha = alpha; BlurSigma = sigma; Blend = bl; Filter = filter }
                    count <- count + 1
                | Protocol.OpPopLayer      -> sink.PopLayer(); count <- count + 1
                | op -> raise (DecodeFailure(at, sprintf "unknown opcode 0x%02X" op))
            sink.EndFrame()
            Decoded count
        with DecodeFailure(off, msg) -> Malformed(off, msg)
