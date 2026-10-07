namespace FSharp.Graphics.Core

/// Corner radii for rounded rectangles
[<Struct>]
type CornerRadius = {
    TopLeft: float32
    TopRight: float32
    BottomRight: float32
    BottomLeft: float32
}

module CornerRadius =
    let zero = { TopLeft = 0.0f; TopRight = 0.0f; BottomRight = 0.0f; BottomLeft = 0.0f }
    let all r = { TopLeft = r; TopRight = r; BottomRight = r; BottomLeft = r }
    let horizontal l r = { TopLeft = l; TopRight = r; BottomRight = r; BottomLeft = l }
    let vertical t b = { TopLeft = t; TopRight = t; BottomRight = b; BottomLeft = b }
    let create tl tr br bl = { TopLeft = tl; TopRight = tr; BottomRight = br; BottomLeft = bl }

    let All = all
    let Create = create
    let Zero = zero
