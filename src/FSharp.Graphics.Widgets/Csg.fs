namespace FSharp.Graphics.Widgets

open System
open System.Collections.Generic
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

type CsgOp =
    | Union
    | Intersect
    | Subtract
    /// Polynomial smooth minimum; `k` = blend radius in pixels.
    | SmoothUnion of k: float32

/// Constructive solid geometry over two SDFs. `Sdf` is exact for hit-testing and for any
/// backend that evaluates SDFs. Hosts that only receive the wire `ShapeGeometry` see a
/// `GOpaque` and draw its bounds, so for *rendering* use `Csg.bake`, which turns the field into
/// a closed polygon (marching squares) that every backend draws exactly.
type CsgShape(op: CsgOp, a: ISdfShape, b: ISdfShape) =
    let bounds =
        match op with
        | Union -> Rect.union a.Bounds b.Bounds
        | SmoothUnion k -> Rect.expand k (Rect.union a.Bounds b.Bounds)
        | Intersect -> Rect.intersect a.Bounds b.Bounds
        | Subtract -> a.Bounds
    let tag, k =
        match op with
        | Union -> 0, 0.0f | Intersect -> 1, 0.0f | Subtract -> 2, 0.0f | SmoothUnion k -> 3, k
    interface ISdfShape with
        member _.Sdf(p) =
            let d1 = a.Sdf p
            let d2 = b.Sdf p
            match op with
            | Union -> Scalar.min' d1 d2
            | Intersect -> Scalar.max' d1 d2
            | Subtract -> Scalar.max' d1 (-d2)
            | SmoothUnion k ->
                let h = Scalar.max' (k - abs (d1 - d2)) 0.0f / k
                Scalar.min' d1 d2 - h * h * k * 0.25f
        member _.Bounds = bounds
        member _.CacheKey = HashCode.Combine(tag, a.CacheKey, b.CacheKey, k)
        member _.Geometry = GOpaque bounds

module Csg =
    let combine (op: CsgOp) (a: ISdfShape) (b: ISdfShape) : ISdfShape = CsgShape(op, a, b) :> ISdfShape
    let union a b = combine Union a b
    let subtract a b = combine Subtract a b
    let intersect a b = combine Intersect a b
    let smooth k a b = combine (SmoothUnion k) a b

    /// Extract the outer contours of `sdf` as polygons (marching squares, `step` px grid).
    /// Interior holes are not representable by a single-polygon shape and are dropped.
    let contours (sdf: ISdfShape) (step: float32) : Vec2[] list =
        let b = Rect.expand (step * 2.0f) sdf.Bounds
        let nx = int (Scalar.ceil (b.W / step)) + 2
        let ny = int (Scalar.ceil (b.H / step)) + 2
        let px i = b.X + float32 i * step
        let py j = b.Y + float32 j * step
        let d = Array.init (nx * ny) (fun k -> sdf.Sdf(Vec2.create (px (k % nx)) (py (k / nx))))
        let at i j = d.[j * nx + i]
        let pos = Dictionary<int, Vec2>()
        let adj = Dictionary<int, ResizeArray<int>>()
        let keyH i j = (j * nx + i) * 2          // edge (i,j)-(i+1,j)
        let keyV i j = (j * nx + i) * 2 + 1      // edge (i,j)-(i,j+1)
        let point (key: int) =
            if not (pos.ContainsKey key) then
                let k = key / 2
                let i = k % nx
                let j = k / nx
                let i2, j2 = if key % 2 = 0 then i + 1, j else i, j + 1
                let d0 = at i j
                let d1 = at i2 j2
                let t = if d0 = d1 then 0.5f else Scalar.clamp 0.0f 1.0f (d0 / (d0 - d1))
                pos.[key] <- Vec2.create (px i + (px i2 - px i) * t) (py j + (py j2 - py j) * t)
        let link (u: int) (v: int) =
            point u; point v
            for (x, y) in [ (u, v); (v, u) ] do
                match adj.TryGetValue x with
                | true, l -> l.Add y
                | _ -> adj.[x] <- ResizeArray [ y ]
        for j in 0 .. ny - 2 do
            for i in 0 .. nx - 2 do
                let da, db, dc, dd = at i j, at (i + 1) j, at (i + 1) (j + 1), at i (j + 1)
                let case =
                    (if da < 0.0f then 1 else 0) ||| (if db < 0.0f then 2 else 0)
                    ||| (if dc < 0.0f then 4 else 0) ||| (if dd < 0.0f then 8 else 0)
                let T, R, B, L = keyH i j, keyV (i + 1) j, keyH i (j + 1), keyV i j
                let centerIn = (da + db + dc + dd) * 0.25f < 0.0f
                match case with
                | 1 | 14 -> link L T
                | 2 | 13 -> link T R
                | 3 | 12 -> link L R
                | 4 | 11 -> link R B
                | 6 | 9  -> link T B
                | 7 | 8  -> link L B
                | 5 -> if centerIn then (link T R; link L B) else (link L T; link R B)
                | 10 -> if centerIn then (link L T; link R B) else (link T R; link B L)
                | _ -> ()
        let visited = HashSet<int>()
        let loops = ResizeArray<Vec2[]>()
        for start in List.ofSeq adj.Keys do
            if not (visited.Contains start) then
                let pts = ResizeArray<Vec2>()
                let mutable prev = -1
                let mutable cur = start
                let mutable fin = false
                while not fin do
                    visited.Add cur |> ignore
                    pts.Add pos.[cur]
                    let nb = adj.[cur]
                    let nxt = if nb.[0] <> prev then nb.[0] elif nb.Count > 1 then nb.[1] else -1
                    if nxt = -1 || nxt = start || visited.Contains nxt then fin <- true
                    else
                        prev <- cur
                        cur <- nxt
                if pts.Count >= 3 then loops.Add(pts.ToArray())
        // drop collinear points
        let simplify (p: Vec2[]) =
            let n = p.Length
            [| for i in 0 .. n - 1 do
                let a, c = p.[(i + n - 1) % n], p.[(i + 1) % n]
                let m = p.[i]
                let cross = (m.X - a.X) * (c.Y - a.Y) - (m.Y - a.Y) * (c.X - a.X)
                let len = (Vec2.sub c a).Length
                if len < 1e-6f || abs cross / len > 0.0005f then yield m |]
        loops
        |> Seq.map simplify
        |> Seq.filter (fun p -> p.Length >= 3)
        |> Seq.filter (fun p ->
            // outer loops only: an interior point of the loop (by a scanline) must be inside the shape
            let n = p.Length
            let y = p.[0].Y + step * 0.37f
            let xs = ResizeArray<float32>()
            for i in 0 .. n - 1 do
                let a, c = p.[i], p.[(i + 1) % n]
                if (a.Y <= y) <> (c.Y <= y) then xs.Add(a.X + (y - a.Y) / (c.Y - a.Y) * (c.X - a.X))
            xs.Sort()
            xs.Count < 2 || sdf.Sdf(Vec2.create ((xs.[0] + xs.[1]) * 0.5f) y) < 0.0f)
        |> List.ofSeq

    /// Render a CSG field exactly on every backend by baking it to polygon(s).
    let bake (sdf: ISdfShape) (fill: Paint) (stroke: Stroke option) (step: float32) : Scene =
        match contours sdf step with
        | [] -> Empty
        | [ p ] -> Shape(Path(PolygonSdf p, fill, stroke, None))
        | ps -> Group [ for p in ps -> Shape(Path(PolygonSdf p, fill, stroke, None)) ]
