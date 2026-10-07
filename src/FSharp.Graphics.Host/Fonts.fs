namespace FSharp.Graphics.Host

open System
open System.Buffers.Binary
open System.Collections.Generic
open FSharp.Graphics.Core

/// Minimal TrueType (glyf) reader: outlines + advances + vertical metrics.
/// Enough for the software backend to draw the exact glyph ids the shaper emitted.
/// Handles simple and composite glyphs; no hinting, no CFF (.otf with CFF outlines).
type TtfFont(data: byte[]) =
    let u16 (o: int) = int (BinaryPrimitives.ReadUInt16BigEndian(ReadOnlySpan<byte>(data, o, 2)))
    let i16 (o: int) = int (BinaryPrimitives.ReadInt16BigEndian(ReadOnlySpan<byte>(data, o, 2)))
    let u32 (o: int) = int (BinaryPrimitives.ReadUInt32BigEndian(ReadOnlySpan<byte>(data, o, 4)))

    let tables =
        let d = Dictionary<string, struct (int * int)>()
        let n = u16 4
        for i in 0 .. n - 1 do
            let r = 12 + i * 16
            let tag = Text.Encoding.ASCII.GetString(data, r, 4)
            d.[tag] <- struct (u32 (r + 8), u32 (r + 12))
        d
    let table (tag: string) =
        match tables.TryGetValue tag with
        | true, struct (o, _) -> o
        | _ -> failwithf "font is missing required table '%s' (CFF/.otf outlines are not supported)" tag

    let head = table "head"
    let upem = u16 (head + 18)
    let longLoca = i16 (head + 50) <> 0
    let loca = table "loca"
    let glyf = table "glyf"
    let hhea = table "hhea"
    let hmtx = table "hmtx"
    let numGlyphs = u16 (table "maxp" + 4)
    let numHMetrics = u16 (hhea + 34)
    let cache = Dictionary<int, float32[][]>()

    let glyphOffset (gid: int) =
        if gid < 0 || gid >= numGlyphs then struct (0, 0)
        else
            let a, b =
                if longLoca then u32 (loca + gid * 4), u32 (loca + gid * 4 + 4)
                else u16 (loca + gid * 2) * 2, u16 (loca + gid * 2 + 2) * 2
            struct (glyf + a, b - a)

    /// Glyph points as (x, y, onCurve) per contour, in font units, composites flattened.
    let rec contours (gid: int) (depth: int) : (float32 * float32 * bool)[] list =
        let struct (o, len) = glyphOffset gid
        if len <= 0 || depth > 6 then []
        else
        let nc = i16 o
        if nc >= 0 then
            let ends = Array.init nc (fun i -> u16 (o + 10 + i * 2))
            let npts = if nc = 0 then 0 else ends.[nc - 1] + 1
            let insLen = u16 (o + 10 + nc * 2)
            let mutable p = o + 12 + nc * 2 + insLen
            let flags = Array.zeroCreate<int> npts
            let mutable i = 0
            while i < npts do
                let f = int data.[p]
                p <- p + 1
                flags.[i] <- f
                i <- i + 1
                if f &&& 8 <> 0 then
                    let rep = int data.[p]
                    p <- p + 1
                    for _ in 1 .. rep do
                        if i < npts then flags.[i] <- f
                        i <- i + 1
            let coords (shortBit: int) (sameBit: int) =
                let a = Array.zeroCreate<int> npts
                let mutable v = 0
                for k in 0 .. npts - 1 do
                    let f = flags.[k]
                    if f &&& shortBit <> 0 then
                        let d = int data.[p]
                        p <- p + 1
                        v <- v + (if f &&& sameBit <> 0 then d else -d)
                    elif f &&& sameBit = 0 then
                        v <- v + i16 p
                        p <- p + 2
                    a.[k] <- v
                a
            let xs = coords 2 16
            let ys = coords 4 32
            let mutable start = 0
            [ for e in ends do
                yield Array.init (e - start + 1) (fun k ->
                    let j = start + k
                    float32 xs.[j], float32 ys.[j], (flags.[j] &&& 1 <> 0))
                start <- e + 1 ]
        else
            // composite
            let mutable p = o + 10
            let mutable more = true
            let acc = ResizeArray<(float32 * float32 * bool)[]>()
            while more do
                let fl = u16 p
                let sub = u16 (p + 2)
                p <- p + 4
                let dx, dy =
                    if fl &&& 1 <> 0 then
                        let a = i16 p in let b = i16 (p + 2)
                        p <- p + 4
                        a, b
                    else
                        let a = int (sbyte data.[p]) in let b = int (sbyte data.[p + 1])
                        p <- p + 2
                        a, b
                let f2 (q: int) = float32 (i16 q) / 16384.0f
                let m11, m12, m21, m22 =
                    if fl &&& 8 <> 0 then
                        let s = f2 p in p <- p + 2
                        s, 0.0f, 0.0f, s
                    elif fl &&& 0x40 <> 0 then
                        let sx = f2 p in let sy = f2 (p + 2) in p <- p + 4
                        sx, 0.0f, 0.0f, sy
                    elif fl &&& 0x80 <> 0 then
                        let a = f2 p in let b = f2 (p + 2) in let c = f2 (p + 4) in let d = f2 (p + 6)
                        p <- p + 8
                        a, b, c, d
                    else 1.0f, 0.0f, 0.0f, 1.0f
                // only XY offsets are supported (ARGS_ARE_XY_VALUES); point-matching is rare
                let ox, oy = (if fl &&& 2 <> 0 then float32 dx, float32 dy else 0.0f, 0.0f)
                for c in contours sub (depth + 1) do
                    acc.Add(c |> Array.map (fun (x, y, on) ->
                        (x * m11 + y * m21 + ox, x * m12 + y * m22 + oy, on)))
                more <- fl &&& 0x20 <> 0
            List.ofSeq acc

    /// Flatten quadratic contours to polylines (flat [x0;y0;x1;y1;...], closed implicitly).
    let flatten (pts: (float32 * float32 * bool)[]) : float32[] =
        let n = pts.Length
        if n < 2 then [||]
        else
            let out = ResizeArray<float32>()
            let add (x: float32) (y: float32) = out.Add x; out.Add y
            let get i = pts.[((i % n) + n) % n]
            // start on an on-curve point (or synthesise the midpoint of two off-curve points)
            let firstOn = pts |> Array.tryFindIndex (fun (_, _, on) -> on)
            let sx, sy, startIdx =
                match firstOn with
                | Some i -> let (x, y, _) = pts.[i] in x, y, i + 1
                | None ->
                    let (x0, y0, _) = pts.[0]
                    let (x1, y1, _) = pts.[1 % n]
                    (x0 + x1) * 0.5f, (y0 + y1) * 0.5f, 1
            add sx sy
            let mutable cx, cy = sx, sy
            let total = if firstOn.IsSome then n - 1 else n
            let mutable k = 0
            let quad (x0: float32) (y0: float32) (qx: float32) (qy: float32) (x1: float32) (y1: float32) =
                let steps = 8
                for s in 1 .. steps do
                    let t = float32 s / float32 steps
                    let a = (1.0f - t) * (1.0f - t)
                    let b = 2.0f * (1.0f - t) * t
                    let c = t * t
                    add (a * x0 + b * qx + c * x1) (a * y0 + b * qy + c * y1)
            while k < total do
                let (x, y, on) = get (startIdx + k)
                if on then
                    add x y
                    cx <- x; cy <- y
                    k <- k + 1
                else
                    let (nx, ny, non) = get (startIdx + k + 1)
                    if non && k + 1 < total + (if firstOn.IsSome then 1 else 0) then
                        quad cx cy x y nx ny
                        cx <- nx; cy <- ny
                        k <- k + 2
                    else
                        let ex, ey = (if non then nx, ny else (x + nx) * 0.5f, (y + ny) * 0.5f)
                        quad cx cy x y ex ey
                        cx <- ex; cy <- ey
                        k <- k + (if non then 2 else 1)
            // close back to the start
            if firstOn.IsNone then quad cx cy (let (x, y, _) = pts.[0] in x) (let (_, y, _) = pts.[0] in y) sx sy
            out.ToArray()

    member _.UnitsPerEm = upem
    member _.Ascender = i16 (hhea + 4)
    member _.Descender = i16 (hhea + 6)
    member _.LineGap = i16 (hhea + 8)
    member _.GlyphCount = numGlyphs
    member _.Data = data

    /// Advance width in font units.
    member _.Advance(gid: int) : int =
        let g = min gid (numHMetrics - 1)
        u16 (hmtx + g * 4)

    /// Flattened outline polylines in font units (y up). Cached per glyph.
    member _.Outline(gid: int) : float32[][] =
        match cache.TryGetValue gid with
        | true, o -> o
        | _ ->
            let o = contours gid 0 |> List.map (fun c -> flatten c) |> List.filter (fun a -> a.Length >= 6) |> Array.ofList
            cache.[gid] <- o
            o

/// FontId -> font bytes (+ parsed outline reader). One store feeds the text engine, the
/// software backend and the Skia backend, so glyph ids always refer to the same font.
type FontStore() =
    let bytes  = Dictionary<uint64, byte[]>()
    let parsed = Dictionary<uint64, TtfFont>()
    let key (FontId id) = id

    /// The font used when a FontId is unknown.
    member val Default : FontId = FontId 0UL with get, set

    member s.Register(id: FontId, data: byte[]) =
        bytes.[key id] <- data
        parsed.Remove(key id) |> ignore

    member s.RegisterFile(id: FontId, path: string) = s.Register(id, IO.File.ReadAllBytes path)

    member s.TryBytes(id: FontId) : byte[] option =
        match bytes.TryGetValue(key id) with
        | true, b -> Some b
        | _ ->
            match bytes.TryGetValue(key s.Default) with
            | true, b -> Some b
            | _ -> None

    member s.TryFont(id: FontId) : TtfFont option =
        let k = if bytes.ContainsKey(key id) then key id else key s.Default
        match parsed.TryGetValue k with
        | true, f -> Some f
        | _ ->
            match bytes.TryGetValue k with
            | true, b ->
                let f = TtfFont b
                parsed.[k] <- f
                Some f
            | _ -> None

/// Finds a usable default font on the current machine (dev convenience; production apps
/// should register their own bytes).
module FontLocator =
    let private candidates (baseDir: string) =
        let walkUp =
            Seq.unfold (fun (d: string) ->
                if isNull d then None else Some (d, IO.Path.GetDirectoryName d)) baseDir
            |> Seq.truncate 8
            |> Seq.collect (fun d ->
                [ IO.Path.Combine(d, "lib", "GlyphAura", "tests", "GlyphAura.Tests", "TestData", "DejaVuSans.ttf")
                  IO.Path.Combine(d, "TestData", "DejaVuSans.ttf")
                  IO.Path.Combine(d, "DejaVuSans.ttf") ])
        Seq.concat [
            (match Environment.GetEnvironmentVariable "FSG_FONT" with null | "" -> Seq.empty | p -> Seq.singleton p)
            walkUp
            Seq.ofList [
                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                "/usr/share/fonts/dejavu/DejaVuSans.ttf"
                "/usr/share/fonts/TTF/DejaVuSans.ttf"
                "/Library/Fonts/Arial Unicode.ttf"
                "C:\\Windows\\Fonts\\arial.ttf" ] ]

    /// First existing candidate path (env FSG_FONT wins).
    let tryFind () : string option =
        candidates AppContext.BaseDirectory |> Seq.tryFind IO.File.Exists

    /// FontStore with the default font (id 0) loaded; fails with a helpful message otherwise.
    let loadDefault () : FontStore =
        let store = FontStore()
        match tryFind () with
        | Some path -> store.RegisterFile(FontId 0UL, path)
        | None -> failwith "No TrueType font found. Set FSG_FONT=/path/to/font.ttf (DejaVuSans.ttf recommended)."
        store
