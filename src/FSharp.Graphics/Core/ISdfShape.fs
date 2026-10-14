/// ISdfShape interface, BoundingBox helpers, and ShapeCache.
namespace FSharp.Graphics

open System.Collections.Generic

// ─────────────────────────────────────────────────────────────────────────────
// ISdfShape
// ─────────────────────────────────────────────────────────────────────────────

/// Every vector primitive implements this interface so the renderer treats all
/// shapes uniformly.  The SDF value is negative inside the shape, zero on the
/// boundary, and positive outside.
type ISdfShape =
    /// Signed distance from point p to the shape boundary.
    abstract member Sdf       : p: Vec2 -> float32
    /// Tight axis-aligned bounding box (may be approximate for paths).
    abstract member Bounds    : Rect
    /// Deterministic cache key (used by ShapeCache).
    abstract member CacheKey  : string

// ─────────────────────────────────────────────────────────────────────────────
// BoundingBox helpers
// ─────────────────────────────────────────────────────────────────────────────

module BoundingBox =
    /// Expand a bounding box by a uniform margin on all sides.
    let inline expand (margin: float32) (r: Rect) = r.Expand margin

    /// Tightest AABB that contains all the given points.
    let ofPoints (pts: Vec2 array) =
        if pts.Length = 0 then Rect.Zero
        else
            let mutable minX = pts.[0].X
            let mutable minY = pts.[0].Y
            let mutable maxX = pts.[0].X
            let mutable maxY = pts.[0].Y
            for i = 1 to pts.Length - 1 do
                let p = pts.[i]
                if p.X < minX then minX <- p.X
                if p.Y < minY then minY <- p.Y
                if p.X > maxX then maxX <- p.X
                if p.Y > maxY then maxY <- p.Y
            { X = minX; Y = minY; W = maxX - minX; H = maxY - minY }

// ─────────────────────────────────────────────────────────────────────────────
// DrawList
// ─────────────────────────────────────────────────────────────────────────────

/// A positioned glyph (used by TextNode and the draw-list encoder).
[<Struct>]
type GlyphPosition =
    { GlyphId  : uint32
      X        : float32
      Y        : float32
      /// Advance width (for cursor positioning).
      AdvanceX : float32 }

/// A single GPU-ready render command.
[<NoComparison>]
type DrawCommand =
    | DrawShape of shape: ISdfShape * paint: Paint * stroke: Stroke option * shadow: Shadow option
    | DrawTextBatch of glyphs: GlyphPosition array * fontId: uint64 * size: float32 * color: Color
    | DrawImage    of imageId: ImageId * destRect: Rect * opacity: float32
    | PushClip     of clipRect: Rect
    | PopClip
    | SetTransform of matrix: Matrix3x2
    | SetOpacity   of alpha: float32
    | BeginFrame
    | EndFrame

/// Ordered list of GPU-ready draw commands produced by the layout/render pass.
type DrawList = DrawCommand array

// ─────────────────────────────────────────────────────────────────────────────
// ShapeCache
// ─────────────────────────────────────────────────────────────────────────────

/// Thread-unsafe (single-threaded by design for WASM) LRU cache that stores
/// the DrawList produced for a given ISdfShape so re-layout is allocation-free
/// for unchanged shapes.
type ShapeCache(capacity: int) =
    let capacity = max 1 capacity
    // Dictionary for O(1) lookup; insertion-ordered list approximates LRU.
    let table   = Dictionary<string, DrawList>(capacity)
    let order   = ResizeArray<string>(capacity)

    let evictOldest () =
        if order.Count > 0 then
            let key = order.[0]
            order.RemoveAt 0
            table.Remove key |> ignore

    /// Try to retrieve the cached DrawList for a shape.
    member _.TryGet(key: string) : DrawList voption =
        match table.TryGetValue key with
        | true, dl -> ValueSome dl
        | _        -> ValueNone

    /// Store (or refresh) a DrawList entry.
    member _.Put(key: string, dl: DrawList) =
        if table.ContainsKey key then
            // Move to most-recently-used position.
            let idx = order.IndexOf key
            if idx >= 0 then order.RemoveAt idx
            order.Add key
        else
            if table.Count >= capacity then evictOldest ()
            order.Add key
        table.[key] <- dl

    member _.Count = table.Count
    member _.Clear() = table.Clear(); order.Clear()

    /// Shared application-level cache (128 entries – enough for typical UIs).
    static member val Shared = ShapeCache(128)
