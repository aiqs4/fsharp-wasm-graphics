namespace FSharp.Graphics.Core

open FSharp.Math

/// Opaque image handle
[<Struct>]
type ImageId = ImageId of uint32

/// Opaque font handle
[<Struct>]
type FontId = FontId of uint64

/// Cursor kinds for reactive regions
[<RequireQualifiedAccess>]
type CursorKind =
    | Default
    | Pointer
    | Text
    | Move
    | ResizeNS
    | ResizeEW
    | ResizeNWSE
    | ResizeNESW
    | Crosshair
    | NotAllowed
    | Grab
    | Grabbing

/// Blend modes for compositing a layer onto its backdrop (W3C Compositing Level 1, separable)
type Blend =
    | Normal | Multiply | Screen | Overlay
    | Darken | Lighten | ColorDodge | ColorBurn
    | HardLight | SoftLight | Difference | Exclusion

/// 4x5 row-major colour matrix (20 floats): rows produce R,G,B,A from [r g b a 1] in 0..1 space.
type ColorMatrix = float32 array
