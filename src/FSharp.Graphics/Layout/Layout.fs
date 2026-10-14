/// Layout engine – pure recursive pass that assigns absolute Rect values to
/// every node in the scene graph.
namespace FSharp.Graphics.Layout

open FSharp.Graphics
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open GlyphAuro

// ─────────────────────────────────────────────────────────────────────────────
// LayoutResult
// ─────────────────────────────────────────────────────────────────────────────

/// A scene node paired with its computed absolute bounding rect.
[<NoComparison>]
type LayoutResult =
    { Scene  : Scene
      Bounds : Rect }

// ─────────────────────────────────────────────────────────────────────────────
// LayoutEngine
// ─────────────────────────────────────────────────────────────────────────────

module LayoutEngine =

    // ── Size measurement ──────────────────────────────────────────────────────

    /// Return the intrinsic size of a scene node given an optional available
    /// width/height constraint.  Returns the preferred size as a Vec2.
    let rec measure (scene: Scene) (available: Vec2) : Vec2 =
        match scene with
        | Empty       -> Vec2.Zero

        | Shape p     ->
            let b = p.Bounds
            Vec2.Create(b.W, b.H)

        | Text node   ->
            let maxW =
                match node.MaxWidth with
                | ValueSome w -> w
                | ValueNone   -> available.X
            let constrained = { node with MaxWidth = ValueSome maxW }
            let _, bounds = TextNode.layout constrained
            Vec2.Create(bounds.W, bounds.H)

        | Image(_, destRect, _) ->
            Vec2.Create(destRect.W, destRect.H)

        | RichText(node, _) ->
            // Use the text layout to determine size.
            let _, bounds = TextNode.layout node
            Vec2.Create(bounds.W, bounds.H)

        | Group children ->
            // Group has no layout; size is the union of all children measured
            // at the same origin.
            children
            |> List.fold (fun acc c ->
                let s = measure c available
                Vec2.Create(max acc.X s.X, max acc.Y s.Y)) Vec2.Zero

        | Stack(dir, gap, alignment, children) ->
            measureStack dir gap alignment children available

        | Clip(region, _) ->
            Vec2.Create(region.W, region.H)

        | Padding(insets, child) ->
            let inner = Vec2.Create(
                            max 0.0f (available.X - insets.Horizontal),
                            max 0.0f (available.Y - insets.Vertical))
            let s = measure child inner
            Vec2.Create(s.X + insets.Horizontal, s.Y + insets.Vertical)

        | Transform(_, child) ->
            measure child available   // transform doesn't change intrinsic size

        | Opacity(_, child)
        | Reactive(_, child)
        | Animated(_, child) ->
            measure child available

    and private measureStack dir gap _alignment children available =
        match dir with
        | ZStack ->
            children
            |> List.fold (fun acc c ->
                let s = measure c available
                Vec2.Create(max acc.X s.X, max acc.Y s.Y)) Vec2.Zero

        | Horizontal ->
            let mutable w = 0.0f
            let mutable h = 0.0f
            let count = List.length children
            for i = 0 to count - 1 do
                let c = List.item i children
                let avail = Vec2.Create(max 0.0f (available.X - w), available.Y)
                let s = measure c avail
                w <- w + s.X + (if i < count - 1 then gap else 0.0f)
                if s.Y > h then h <- s.Y
            Vec2.Create(w, h)

        | Vertical ->
            let mutable w = 0.0f
            let mutable h = 0.0f
            let count = List.length children
            for i = 0 to count - 1 do
                let c = List.item i children
                let avail = Vec2.Create(available.X, max 0.0f (available.Y - h))
                let s = measure c avail
                h <- h + s.Y + (if i < count - 1 then gap else 0.0f)
                if s.X > w then w <- s.X
            Vec2.Create(w, h)

    // ── Layout pass ───────────────────────────────────────────────────────────

    /// Recursively assign absolute Rect values.
    /// `origin` is the top-left corner in screen space; `available` is the
    /// maximum size available to this node.
    let rec layout (scene: Scene) (origin: Vec2) (available: Vec2) : LayoutResult =
        match scene with
        | Empty ->
            { Scene = scene; Bounds = { X = origin.X; Y = origin.Y; W = 0.0f; H = 0.0f } }

        | Shape p ->
            let b = p.Bounds
            // Shapes carry their own position encoded in the SDF bounds.
            // We translate to origin so the layout engine can reposition them.
            let positioned =
                let dx = origin.X - b.X
                let dy = origin.Y - b.Y
                if dx = 0.0f && dy = 0.0f then scene
                else
                    let tx = Matrix3x2.Translation(dx, dy)
                    // Wrap in a Transform so the encoder can apply it.
                    Transform(tx, scene)
            { Scene = positioned; Bounds = { X = origin.X; Y = origin.Y; W = b.W; H = b.H } }

        | Text node ->
            let maxW =
                match node.MaxWidth with
                | ValueSome w -> w
                | ValueNone   -> available.X
            let constrained = { node with MaxWidth = ValueSome maxW }
            let _, bounds   = TextNode.layout constrained
            { Scene = Text constrained
              Bounds = { X = origin.X; Y = origin.Y; W = bounds.W; H = bounds.H } }

        | Image(_, destRect, _) ->
            { Scene = scene
              Bounds = { X = origin.X; Y = origin.Y; W = destRect.W; H = destRect.H } }

        | RichText(node, _) ->
            let _, bounds = TextNode.layout node
            // Position the RichText at origin.
            { Scene = RichText(node, Vec2.Create(origin.X, origin.Y))
              Bounds = { X = origin.X; Y = origin.Y; W = bounds.W; H = bounds.H } }

        | Group children ->
            // All children share the same origin.
            let results =
                children |> List.map (fun c -> layout c origin available)
            let bounds =
                results
                |> List.fold (fun acc r ->
                    if acc = Rect.Zero then r.Bounds
                    else Rect.Union acc r.Bounds) Rect.Zero
            { Scene = Group(results |> List.map (fun r -> r.Scene))
              Bounds = bounds }

        | Stack(dir, gap, alignment, children) ->
            layoutStack dir gap alignment children origin available

        | Clip(region, child) ->
            let clippedOrigin = Vec2.Create(region.X, region.Y)
            let clippedAvail  = Vec2.Create(region.W, region.H)
            let childResult   = layout child clippedOrigin clippedAvail
            { Scene  = Clip(region, childResult.Scene)
              Bounds = Rect.Intersect { X = origin.X; Y = origin.Y; W = available.X; H = available.Y } region }

        | Padding(insets, child) ->
            let innerOrigin = Vec2.Create(origin.X + insets.Left, origin.Y + insets.Top)
            let innerAvail  = Vec2.Create(
                                max 0.0f (available.X - insets.Horizontal),
                                max 0.0f (available.Y - insets.Vertical))
            let childResult = layout child innerOrigin innerAvail
            let outerSize   = measure scene available
            { Scene  = Padding(insets, childResult.Scene)
              Bounds = { X = origin.X; Y = origin.Y; W = outerSize.X; H = outerSize.Y } }

        | Transform(matrix, child) ->
            let childResult = layout child origin available
            { Scene  = Transform(matrix, childResult.Scene)
              Bounds = childResult.Bounds }

        | Opacity(alpha, child) ->
            let childResult = layout child origin available
            { Scene  = Opacity(alpha, childResult.Scene)
              Bounds = childResult.Bounds }

        | Reactive(handler, child) ->
            let childResult = layout child origin available
            { Scene  = Reactive(handler, childResult.Scene)
              Bounds = childResult.Bounds }

        | Animated(trans, child) ->
            let childResult = layout child origin available
            { Scene  = Animated(trans, childResult.Scene)
              Bounds = childResult.Bounds }

    and private layoutStack dir gap alignment children origin available =
        let sizes =
            children |> List.map (fun c -> measure c available)

        let results =
            match dir with
            | ZStack ->
                children |> List.mapi (fun i c ->
                    layout c origin available)

            | Horizontal ->
                let mutable penX = origin.X
                children |> List.mapi (fun i c ->
                    let sz = sizes.[i]
                    let childOrigin =
                        let y =
                            match alignment with
                            | Alignment.Start   -> origin.Y
                            | Alignment.End     -> origin.Y + available.Y - sz.Y
                            | Alignment.Center  -> origin.Y + (available.Y - sz.Y) * 0.5f
                            | Alignment.Stretch -> origin.Y
                        Vec2.Create(penX, y)
                    let childAvail =
                        match alignment with
                        | Alignment.Stretch -> Vec2.Create(sz.X, available.Y)
                        | _       -> sz
                    let r = layout c childOrigin childAvail
                    penX <- penX + sz.X + gap
                    r)

            | Vertical ->
                let mutable penY = origin.Y
                children |> List.mapi (fun i c ->
                    let sz = sizes.[i]
                    let childOrigin =
                        let x =
                            match alignment with
                            | Alignment.Start   -> origin.X
                            | Alignment.End     -> origin.X + available.X - sz.X
                            | Alignment.Center  -> origin.X + (available.X - sz.X) * 0.5f
                            | Alignment.Stretch -> origin.X
                        Vec2.Create(x, penY)
                    let childAvail =
                        match alignment with
                        | Alignment.Stretch -> Vec2.Create(available.X, sz.Y)
                        | _       -> sz
                    let r = layout c childOrigin childAvail
                    penY <- penY + sz.Y + gap
                    r)

        let bounds =
            results
            |> List.fold (fun acc r ->
                if acc = Rect.Zero then r.Bounds
                else Rect.Union acc r.Bounds) Rect.Zero

        { Scene  = Stack(dir, gap, alignment, results |> List.map (fun r -> r.Scene))
          Bounds = bounds }

    /// Entry point: lay out the entire scene in a viewport.
    let layoutScene (scene: Scene) (viewport: Rect) : LayoutResult =
        layout scene
               (Vec2.Create(viewport.X, viewport.Y))
               (Vec2.Create(viewport.W, viewport.H))
