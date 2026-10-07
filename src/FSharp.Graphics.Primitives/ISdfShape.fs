namespace FSharp.Graphics.Primitives

open FSharp.Math
open FSharp.Graphics.Core

/// Closed, serialisable description of a shape. Travels over the command stream so a host can
/// draw the shape natively (Skia path, Canvas2D path, GPU primitive) without evaluating the SDF.
type ShapeGeometry =
    | GRect    of bounds: Rect * radius: CornerRadius
    | GCircle  of center: Vec2 * radius: float32
    | GEllipse of center: Vec2 * rx: float32 * ry: float32
    | GPolygon of vertices: Vec2 array
    | GLine    of a: Vec2 * b: Vec2 * thickness: float32
    /// Custom SDF without a closed description; hosts approximate it by its bounds.
    | GOpaque  of bounds: Rect

/// Interface for signed-distance-field shapes
type ISdfShape =
    /// Signed distance to the boundary (negative inside). Must be ~Euclidean (|grad| ~ 1):
    /// anti-aliasing, strokes and shadows all assume distances measured in pixels.
    abstract Sdf: point: Vec2 -> float32
    /// Axis-aligned bounding box
    abstract Bounds: Rect
    /// Opaque cache key for shape identity
    abstract CacheKey: int
    abstract Geometry: ShapeGeometry
