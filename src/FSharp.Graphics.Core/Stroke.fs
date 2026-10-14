namespace FSharp.Graphics.Core

open FSharp.Math

/// Line cap style
type LineCap = Butt | Round | Square

/// Line join style
type LineJoin = Miter | RoundJoin | Bevel

/// Stroke definition
[<Struct>]
type Stroke = {
    Paint     : Paint
    Width     : float32
    Cap       : LineCap
    Join      : LineJoin
    MiterLimit: float32
}

module Stroke =
    let create paint width =
        { Paint = paint; Width = width; Cap = Butt; Join = Miter; MiterLimit = 4.0f }
    let solid color width =
        create (Solid color) width
    let withCap cap (s: Stroke) = { s with Cap = cap }
    let withJoin join (s: Stroke) = { s with Join = join }
