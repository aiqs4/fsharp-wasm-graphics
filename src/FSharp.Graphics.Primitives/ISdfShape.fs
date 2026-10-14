namespace FSharp.Graphics.Primitives

open FSharp.Math

/// Interface for signed-distance-field shapes
type ISdfShape =
    /// Signed distance from point to shape boundary (negative = inside)
    abstract Sdf: point: Vec2 -> float32
    /// Axis-aligned bounding box
    abstract Bounds: Rect
    /// Opaque cache key for shape identity
    abstract CacheKey: int
