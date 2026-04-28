# Compiler Handoff Guide — fsharp-wasm-graphics × fsharp-wasm

> **Compiler tag:** `sprint-26b`  
> **Date:** May 2026  
> **Status:** Ready for M6 (static render milestone) integration

Use this document to set up the WasmGC compiler and understand what works, what to
avoid, and how to structure your code for maximum compatibility.

---

## Quick Start

### Prerequisites

```bash
# From fsharp-wasm repo root:
dotnet build src/Fable.WasmGc.fsproj -c Release -v q
# → Build succeeded. 0 Warning(s) 0 Error(s)
```

### Compiling a graphics project

```bash
export FSHARP_WASM_DIR=/path/to/fsharp-wasm   # points to the fsharp-wasm source tree
# Use the fsharp-wasm-runtime build pipeline to compile your .fsproj:
node build/build.js                             # from fsharp-wasm-runtime root
# or compile directly:
dotnet run --project "$FSHARP_WASM_DIR/tools/FSharp.Wasm.Cli" -- \
    YourProject.fsproj --outDir dist/ --noCache
```

### Import / Export ABI

```fsharp
open Fable.Core

// Import from JavaScript / glue layer:
[<Import("myJsFunction", "../../my-glue.js")>]
let myJsFunction (x: float32) : float32 = nativeOnly

// Export to JavaScript (use the module-level function name directly):
[<CompiledName("fsg_render_frame")>]     // optional: override the exported WASM name
let renderFrame (width: int) (height: int) : unit =
    // ...
```

---

## Feature Support Matrix (sprint-26b)

| Feature | Status | Notes |
|---------|--------|-------|
| **F1 — WasmImport** | ✅ Full | `[<Import("name","module")>]` + `nativeOnly` |
| **F1 — WasmExport** | ✅ Full | Top-level `let` compiles to exported WASM func; use `[<CompiledName>]` for a custom name |
| **F2 — Span<T> / linear memory** | ✅ Infrastructure (sprint-26b) | WasmGC linear memory instructions (`MemLoad`/`MemStore`/`MemGrow`/`MemSize`) fully implemented in compiler and encoder. F# surface (`Span<T>`, pointer types, allocator) is sprint-27. Use `WasmMemory.fs` bindings from fsharp-wasm-runtime for raw access. |
| **F3 — Generic monomorphization** | ✅ Full | Fable + our generic specialization handle this automatically |
| **F4 — DU structural equality** | ✅ Full | `WasmGcEquality.fs` generates equality functions |
| **F5 — Separate compilation** | ⚠️ Partial (sprint-26b) | Multi-project `<ProjectReference>` graphs are supported at the Fable level. Cross-module calls work via Fable's `Import` edges. True separate/incremental `.wasm` linking is sprint-27+. |
| **F6 — F64 trig** | ✅ Full | `Math.Sin`/`Cos`/`Tan`/`Atan2`/`Exp`/`Log` via software polynomial helpers |
| **F6 — F32 trig (MathF)** | ✅ Full (sprint-26b) | `MathF.Sin`/`Cos`/`Tan`/`Atan2` promote→call→demote automatically |
| **F6 — sqrt / abs / floor / ceil / trunc** | ✅ Full | Native WASM instructions for both `float32` and `float64` |
| **Reinterpret casts** | ✅ Full (sprint-26b) | `BitConverter.Int32BitsToSingle` / `SingleToInt32Bits` etc. → native WASM reinterpret ops |

---

## What Works

### Math (float32 and float64)

All standard math operations work for both `float` (f64) and `float32` (f32):

```fsharp
open System

// float (f64)
let y = Math.Sin(x)          // ✅
let c = Math.Cos(theta)       // ✅
let a = Math.Atan2(y, x)     // ✅
let e = Math.Exp(t)           // ✅
let l = Math.Log(v)           // ✅
let s = Math.Sqrt(r)          // ✅ (native WASM f64.sqrt)

// float32 (f32) — sprint-26b
let y32 = MathF.Sin(x32)     // ✅ promote → $mathSin → demote
let c32 = MathF.Cos(t32)     // ✅
let a32 = MathF.Atan2(y, x)  // ✅
let s32 = MathF.Sqrt(r32)    // ✅ (native WASM f32.sqrt)
```

### BitConverter reinterpret casts

```fsharp
open System

// All four reinterpret directions work (sprint-26b):
let f : float32 = BitConverter.Int32BitsToSingle(bits)   // ✅ i32 → f32
let i : int     = BitConverter.SingleToInt32Bits(f)      // ✅ f32 → i32
let d : float   = BitConverter.Int64BitsToDouble(bits64) // ✅ i64 → f64
let l : int64   = BitConverter.DoubleToInt64Bits(d)      // ✅ f64 → i64

// Also supported:
let fu : float32 = BitConverter.UInt32BitsToSingle(u)    // ✅ (same as Int32BitsToSingle)
let u  : uint32  = BitConverter.SingleToUInt32Bits(f)    // ✅
```

### Type system

- Records, discriminated unions, tuples — all work
- Structural equality on DUs via generated `$eq_*` functions
- Generic type parameters are monomorphized by Fable
- Interfaces / virtual dispatch work via WasmGC struct casts

---

## Known Limitations

### No Span<T> or linear memory (F2)

### Linear memory (F2 — sprint-26b)

The compiler now emits WasmGC linear memory instructions. The module-level flag
`HasLinearMemory = true` causes a `(memory 1)` section to be emitted. Instructions
available:

```fsharp
// Low-level (via WasmMemory.fs in fsharp-wasm-runtime):
open Browser.WasmMemory
let v = memLoad32 addr           // i32.load
memStore32 addr value            // i32.store
let page = memGrow 1             // memory.grow
let size = memSize ()            // memory.size
```

A high-level F# `Span<T>` surface and managed arena allocator are on the sprint-27 roadmap.
For M6, use `byte[]` / `int[]` for managed buffers or raw `memLoad`/`memStore` for
high-performance pixel packing.

### Single-project compilation only (F5)

Multi-project `<ProjectReference>` graphs work at the Fable level — cross-module
calls are resolved correctly through Fable's `Import` edges. Full separate/incremental
`.wasm` object linking (like wasm-ld) is not yet implemented and is sprint-27+.
For M6, all source files in one `.fsproj` is the most reliable path.

### No exception handling

`try / with / raise` are not yet implemented. Structure your M6 render path as
a pure computation that never throws (use `Result<T, E>` or option types instead).

---

## Example Project Structure

A minimal graphics module that compiles to WasmGC:

```
YourRenderer/
  YourRenderer.fsproj    # references fsharp-wasm compiler
  Math/
    Scalar.fs             # MathF wrappers (just use them directly)
    Vec2.fs
    Vec3.fs
    Mat3x2.fs
  Render/
    Encode.fs             # uses BitConverter.SingleToInt32Bits → ✅
    Internal.fs
  Tea/
    Runtime.fs            # uses BitConverter.Int32BitsToSingle → ✅
    Tea.fs
```

```xml
<!-- YourRenderer.fsproj (minimal) -->
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <LangVersion>preview</LangVersion>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Math/Scalar.fs" />
    <Compile Include="Math/Vec2.fs" />
    <!-- ... -->
    <Compile Include="Tea/Runtime.fs" />
    <PackageReference Include="Fable.Core" Version="4.*" />
  </ItemGroup>
</Project>
```

---

## Compiler ID / Reproducibility

```
Compiler tag:  sprint-26b
F# compiler:   /home/alex/work/src/fsharp-wasm/vendor/fsharp/artifacts/bin/fsc/Release/net10.0/fsc.dll
Fable version: 4.29.0
QuickTests:    544/544 pass
```

To pin your build to this exact compiler state:

```bash
cd /path/to/fsharp-wasm
git checkout sprint-26b
dotnet build src/Fable.WasmGc.fsproj -c Release
```

---

## Reporting Issues

If you hit a compilation crash or unexpected `unreachable` trap at runtime:

1. Reduce to a minimal `.fsx` or single `.fs` repro
2. Run with `--noCache` to avoid stale transforms
3. Add the repro + WAT output to a new file in `docs/internal/_NOTES/`
4. Tag the compiler team in the issue

The most common cause of `unreachable` at runtime is an unhandled `Import` call
pattern emitting `Nop` instead of a real instruction. Adding a `[<WasmGcWarning>]`
attribute is on the roadmap; for now check the build log for `WARNING: unhandled import`.
