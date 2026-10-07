/// FSG command-stream protocol, version 2. SINGLE SOURCE OF TRUTH for the wire format:
/// the encoder, the .NET decoder and the JS/browser host all derive from this table.
///
/// All values little-endian. Colours are 4 bytes RGBA, straight (non-premultiplied) alpha.
/// Coordinates are float32 in the *local* space established by the enclosing PushTransform
/// chain; PushTransform CONCATENATES (child points go through the matrix, then the parent's).
///
///   0xFE BeginFrame   [version:1 | seq:4]
///   0xFF EndFrame
///   0x01 Fill         [geom | paint]
///   0x02 Stroke       [geom | width:4 | cap:1 | join:1 | miterLimit:4 | paint]     centred on boundary
///   0x03 Shadow       [geom | offsetX:4 | offsetY:4 | blur:4 | spread:4 | color:4 | inner:1]
///                     blur is the CSS blur radius: gaussian sigma = blur / 2
///   0x05 Text         [fontId:8 | sizePx:4 | color:4 | count:4 | count * (glyphId:4 | x:4 | y:4)]
///                     (x,y) = baseline pen position
///   0x06 Image        [imageId:4 | rect:16 | opacity:4]
///   0x10 PushClip     [rect:16]     0x11 PopClip
///   0x12 PushTransform[m11 m12 m21 m22 m31 m32 : 24]     0x13 PopTransform
///   0x14 PushLayer    [alpha:4 | blurSigma:4 | blend:1 | matrixLen:1 | matrixLen*4]   0x15 PopLayer
///
///   geom  = tag:1 then
///     0 Rect     x y w h  tl tr br bl       (8 x f32)
///     1 Circle   cx cy r
///     2 Ellipse  cx cy rx ry
///     3 Polygon  n:4 | n * (x y)
///     4 Line     ax ay bx by thickness
///     5 Opaque   x y w h                    (custom SDF; host approximates by bounds)
///   paint = tag:1 then
///     0 Solid    rgba:4
///     1 Linear   x0 y0 x1 y1 | n:1 | n * stop
///     2 Radial   cx cy r     | n:1 | n * stop
///     3 Image    imageId:4 | opacity:4
///   stop  = offset:4 | rgba:4
namespace FSharp.Graphics.Render

module Protocol =
    [<Literal>]
    let Version = 2uy

    [<Literal>]
    let OpBeginFrame = 0xFEuy
    [<Literal>]
    let OpEndFrame = 0xFFuy
    [<Literal>]
    let OpFill = 0x01uy
    [<Literal>]
    let OpStroke = 0x02uy
    [<Literal>]
    let OpShadow = 0x03uy
    [<Literal>]
    let OpText = 0x05uy
    [<Literal>]
    let OpImage = 0x06uy
    [<Literal>]
    let OpPushClip = 0x10uy
    [<Literal>]
    let OpPopClip = 0x11uy
    [<Literal>]
    let OpPushTransform = 0x12uy
    [<Literal>]
    let OpPopTransform = 0x13uy
    [<Literal>]
    let OpPushLayer = 0x14uy
    [<Literal>]
    let OpPopLayer = 0x15uy

    [<Literal>]
    let GeomRect = 0uy
    [<Literal>]
    let GeomCircle = 1uy
    [<Literal>]
    let GeomEllipse = 2uy
    [<Literal>]
    let GeomPolygon = 3uy
    [<Literal>]
    let GeomLine = 4uy
    [<Literal>]
    let GeomOpaque = 5uy

    [<Literal>]
    let PaintSolid = 0uy
    [<Literal>]
    let PaintLinear = 1uy
    [<Literal>]
    let PaintRadial = 2uy
    [<Literal>]
    let PaintImage = 3uy
