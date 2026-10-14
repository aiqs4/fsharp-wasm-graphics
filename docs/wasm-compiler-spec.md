# FSharp.Graphics — WasmGC Compiler Integration Spec

**Version:** 1.0  
**Date:** April 2026  
**Status:** Compiler Team Handoff  
**Audience:** FSharp-to-WasmGC compiler team

---

## Overview

The `FSharp.Graphics` library stack is complete and all tests pass on the .NET runtime.  
The library source files are already written to be BCL-minimal — they are ready to compile under the WasmGC backend **today**, pending the compiler features listed in this document.

This spec defines exactly what the compiler must implement to unlock milestones M6 and M7.

---

## Current Status

| Milestone | What | Status |
|-----------|------|--------|
| M1–M5 | Full F# graphics stack, 118 property tests, 18 golden images | ✅ **Done** |
| M6 | First WasmGC binary renders `golden_001.png` pixel-accurately | 🔲 Blocked on compiler |
| M7 | All 18 goldens pass in WASM CI | 🔲 Blocked on M6 |

---

## Required Compiler Features

### F1 — `[<WasmImport>]` / `[<WasmExport>]` Attributes

**Needed by:** `src/FSharp.Graphics/Tea/Runtime.fs`, `src/FSharp.Graphics/Render/Encode.fs`  
**Priority:** CRITICAL (M6 blocker)

The runtime needs to import browser/WASM host functions and export the frame callback.

```fsharp
// Export the main render loop entry point
[<WasmExport("fsg_render_frame")>]
let renderFrame (timestamp: float) : unit = ...

// Import canvas submission from the WASM host
[<WasmImport("env", "submit_frame")>]
extern void submitFrame(nativeint bufPtr, int length)
```

**Contract:**
- `[<WasmExport(name)>]` on a `let` binding → exported WASM function with the given name
- `[<WasmImport(module, name)>]` on an `extern` declaration → imported WASM function
- The generated WASM must match the signature exactly (i32 for int, f32 for float32, etc.)

---

### F2 — `Span<T>` in Linear Memory

**Needed by:** `tests/FSharp.Graphics.GoldenTests/Rasterizer.fs`, `Encode.fs`  
**Priority:** HIGH (M6 blocker for zero-copy command buffer)

The rasterizer writes pixel data into a `byte[]` / `Span<byte>` buffer that is then passed by pointer to the host.

```fsharp
// Zero-copy frame submission — no allocation on the hot path
let submitFrame (pixels: Span<byte>) =
    use pinned = fixed pixels
    submitFrameNative (NativePtr.toNativeInt pinned) pixels.Length
```

**Contract:**
- `Span<T>` must map to a contiguous region in WasmGC linear memory
- `fixed` keyword must pin the span and expose a raw i32 memory offset
- `Array.copy`, `Buffer.BlockCopy` equivalents must work via linear memory copy instruction

---

### F3 — Generic Type Monomorphization

**Needed by:** `src/FSharp.Graphics/Tea/Tea.fs` (`Program<'Model,'Msg>`), `Runtime<'Model,'Msg>`  
**Priority:** HIGH (M6 blocker)

The TEA runtime is generic:

```fsharp
type Program<'Model, 'Msg> = {
    Init   : unit -> 'Model * Cmd<'Msg>
    Update : 'Model -> 'Msg -> 'Model * Cmd<'Msg>
    View   : 'Model -> Scene
}
```

**Contract:**
- All generic type parameters must be monomorphized at the call site
- No boxing of value types (`int`, `float32`, `Vec2`, struct DUs) when used as type arguments
- The consumer application provides concrete type instantiations; the library must not require runtime type info (no `typeof<'T>`, no `Reflection`)

---

### F4 — Structural Equality on Discriminated Unions

**Needed by:** `src/FSharp.Graphics/Scene/Diff.fs`  
**Priority:** MEDIUM (M6 can ship without it using reference equality; M7 requires it for correctness)

The scene diff engine compares `Scene` trees for change detection:

```fsharp
match nodeA, nodeB with
| Rect(a), Rect(b) when a = b -> NoChange   // structural equality
| Text(a), Text(b) when a = b -> NoChange
| _ -> Replace nodeB
```

**Contract:**
- Structural equality (`=`) on DU cases must work without `System.Object.Equals` fallback
- `[<CustomEquality; CustomComparison>]` must be respected
- Equality on nested structs (`Vec2`, `Color`, `Rect`) must be value equality, not reference equality

---

### F5 — Object File Linking / Separate Compilation

**Needed by:** Multi-project solution (`FSharp.Math` → `FSharp.Graphics.Core` → ... → `FSharp.Graphics.Tea`)  
**Priority:** MEDIUM (required for library boundary; M7 prerequisite)

The library is split across 6 projects. Each must compile to its own `.wasm` module or object file that can be linked.

**Contract:**
- Each `.fsproj` compiles to a `.wasm` module (or object for linking)
- `ProjectReference` in `.fsproj` maps to a module import at the WASM level
- The linker must resolve cross-module calls without duplicating code
- `InternalsVisibleTo` attribute must be honoured between linked modules

---

### F6 — `MathF` / Trigonometry Intrinsics

**Needed by:** `src/FSharp.Math/Scalar.fs` (`sin`, `cos`, `sqrt`, `atan2`, `pow`)  
**Priority:** CRITICAL (M6 blocker — all rendering uses trig)

```fsharp
module Scalar =
    let inline sin  (x: float32) = MathF.Sin x
    let inline cos  (x: float32) = MathF.Cos x
    let inline sqrt (x: float32) = MathF.Sqrt x
```

**Contract:**
- `MathF.Sin/Cos/Sqrt/Atan2/Pow` must lower to `f32.sin`, `f32.sqrt` etc. (WasmGC math instructions or WASI imports)
- No `System.Math` (double) fallback
- Results must match IEEE 754 `f32` semantics within 1 ULP

---

## Host ABI Contract (JavaScript / Browser Side)

The WASM module exports one entry point and imports two host functions:

```typescript
// JavaScript host (TypeScript for clarity)
const imports = {
  env: {
    // Called once per frame with a pointer + byte length into WASM linear memory
    submit_frame: (ptr: number, len: number): void => {
      const pixels = new Uint8ClampedArray(wasm.memory.buffer, ptr, len);
      ctx.putImageData(new ImageData(pixels, width, height), 0, 0);
    },
    // High-resolution timestamp in milliseconds (same origin as requestAnimationFrame)
    now_ms: (): number => performance.now(),
  }
};

const { instance } = await WebAssembly.instantiateStreaming(fetch('app.wasm'), imports);

// Start the render loop
function loop(ts: number) {
  instance.exports.fsg_render_frame(ts);
  requestAnimationFrame(loop);
}
requestAnimationFrame(loop);
```

### Memory Layout

The frame buffer is a flat RGBA8 array in linear memory:

```
offset 0: pixel(0,0).R  pixel(0,0).G  pixel(0,0).B  pixel(0,0).A
offset 4: pixel(1,0).R  ...
...
offset (y*width + x)*4 : pixel(x,y)
```

- Width × Height × 4 bytes total
- Row-major, top-left origin
- Alpha is pre-multiplied (as produced by `Color.over`)

---

## Minimal BCL Requirements

The library source uses only the following BCL types. The compiler BCL must provide them:

| Type | Used in | Notes |
|------|---------|-------|
| `System.Math` / `MathF` | `Scalar.fs` | `sin`, `cos`, `sqrt`, `abs`, `floor`, `ceil`, `min`, `max` |
| `System.Array` | Throughout | `Array.zeroCreate`, `Array.copy`, indexing |
| `System.String` | `Scene.fs` (text content only) | UTF-8 encoding; no regex, no formatting |
| `System.Collections.Generic.List<T>` | `Runtime.fs` (message queue) | `Add`, `Count`, iteration |
| `System.Buffers.ArrayPool<T>` | `Rasterizer.fs` | `Shared.Rent`, `Shared.Return` |
| `System.Span<T>` / `ReadOnlySpan<T>` | `Rasterizer.fs`, `Encode.fs` | Linear memory slice |
| `System.MathF.PI` | `Scalar.fs` | Constant |

**Not needed:** reflection, LINQ, threading (`Task`, `async` via `Async<T>`), file I/O, networking, serialization.

---

## Validation: How to Know M6 Is Done

1. Build `demo/FSharp.Graphics.Demo` targeting WasmGC
2. Load in browser via `wasm-pack serve` or equivalent
3. Compare rendered output pixel-for-pixel against `tests/FSharp.Graphics.GoldenTests/golden/001_solid_rect.png`
4. Tolerance: ≤ 2 RGBA8 units per channel per pixel (accounts for float→int rounding differences)
5. Run CI job `.github/workflows/ci.yml` step `golden-wasm`

**The test is:**
```bash
dotnet run --project tests/FSharp.Graphics.GoldenTests -- --wasm-binary path/to/app.wasm
```
This runs the golden comparison against the WASM-rendered output using the same PNG comparison infrastructure already in CI.

---

## M7: All 18 Goldens

Once M6 passes, run all 18:

```bash
dotnet run --project tests/FSharp.Graphics.GoldenTests -- --wasm-binary path/to/app.wasm --all
```

Expected: `18 passed, 0 failed`.

---

## Contact

For questions on the library API, scene graph contract, or pixel test infrastructure:
- See `docs/spec-draft.md` (complete architecture)
- See `src/FSharp.Graphics/Tea/Runtime.fs` (host ABI implementation on .NET side)
- See `tests/FSharp.Graphics.GoldenTests/` (golden test harness)
