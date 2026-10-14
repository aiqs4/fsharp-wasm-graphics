#!/usr/bin/env bash
# build-wasm.sh — compile FSharp.Graphics to WasmGC using the fsharp-wasm compiler.
#
# Requirements:
#   - tools/fsharp-wasm submodule checked out
#   - dotnet SDK ≥ 8.0
#   - node.js ≥ 18
#
# Usage:
#   bash build/build-wasm.sh                   # build only
#   bash build/build-wasm.sh --test            # build + run wasmecto tests
#   bash build/build-wasm.sh --out dist/       # specify output directory

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WASM_COMPILER="$REPO_ROOT/tools/fsharp-wasm"
WASM_ECTO="$REPO_ROOT/tools/fsharp-wasmecto"
WASM_PROJECT="$REPO_ROOT/src/FSharp.Graphics.Wasm/FSharp.Graphics.Wasm.fsproj"
WASM_TEST_PROJECT="$REPO_ROOT/tests/FSharp.Graphics.WasmTests/FSharp.Graphics.WasmTests.fsproj"
OUT_DIR="$REPO_ROOT/dist"
RUN_TESTS=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --test) RUN_TESTS=true; shift ;;
        --out)  OUT_DIR="$2"; shift 2 ;;
        *)      echo "Unknown argument: $1"; exit 1 ;;
    esac
done

mkdir -p "$OUT_DIR"

if [[ ! -d "$WASM_COMPILER" ]]; then
    echo "ERROR: tools/fsharp-wasm submodule not found."
    echo "       Run: git submodule update --init tools/fsharp-wasm"
    exit 1
fi

echo "=== Building fsharp-wasm compiler (sprint-26b) ==="
cd "$WASM_COMPILER"
dotnet build src/Fable.WasmGc.fsproj -c Release -v q

echo ""
echo "=== Compiling FSharp.Graphics.Wasm to WasmGC ==="
export FSHARP_WASM_DIR="$WASM_COMPILER"
dotnet run --project "$FSHARP_WASM_DIR/tools/fsharp-wasm-cli" -- \
    "$WASM_PROJECT" --outDir "$OUT_DIR" --noCache --defines FABLE_COMPILER

echo ""
echo "=== WASM build complete ==="
echo "  Binary:      $OUT_DIR/FSharp.Graphics.Wasm.wasm"
echo "  WAT (debug): $OUT_DIR/FSharp.Graphics.Wasm.wat"
echo "  Glue:        $OUT_DIR/glue.mjs"

if [[ "$RUN_TESTS" == "true" ]]; then
    echo ""
    echo "=== Compiling WASM tests ==="
    if [[ ! -d "$WASM_ECTO" ]]; then
        echo "ERROR: tools/fsharp-wasmecto submodule not found."
        exit 1
    fi
    dotnet run --project "$FSHARP_WASM_DIR/tools/fsharp-wasm-cli" -- \
        "$WASM_TEST_PROJECT" --outDir "$OUT_DIR/tests" --noCache --defines FABLE_COMPILER
    echo ""
    echo "=== Running WASM tests via wasmecto ==="
    node "$WASM_ECTO/runner/run.mjs" "$OUT_DIR/tests/FSharp.Graphics.WasmTests.wasm"
fi
