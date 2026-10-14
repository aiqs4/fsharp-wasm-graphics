namespace FSharp.Graphics.Primitives

/// Mutable shape cache with LRU eviction
type ShapeCache(capacity: int) =
    let cache = System.Collections.Generic.Dictionary<int, ISdfShape>(capacity)
    let evictionOrder = System.Collections.Generic.Queue<int>(capacity)

    member _.TryGet(key: int) : ISdfShape option =
        match cache.TryGetValue(key) with
        | true, v -> Some v
        | false, _ -> None

    member _.Put(key: int, shape: ISdfShape) =
        if cache.ContainsKey key then
            cache.[key] <- shape
        else
            if cache.Count >= capacity && evictionOrder.Count > 0 then
                let evict = evictionOrder.Dequeue()
                cache.Remove(evict) |> ignore

            cache.[key] <- shape
            evictionOrder.Enqueue(key)

    member _.Count = cache.Count

    member _.Clear() =
        cache.Clear()
        evictionOrder.Clear()

    static member Shared = ShapeCache(1024)
