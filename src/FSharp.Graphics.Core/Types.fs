namespace FSharp.Graphics.Core

open FSharp.Math

/// Opaque image handle
[<Struct>]
type ImageId = ImageId of uint32

/// Opaque font handle
[<Struct>]
type FontId = FontId of uint64

/// Cursor kinds for reactive regions
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
