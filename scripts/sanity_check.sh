#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
SPEC="$ROOT_DIR/examples/specs/sample.yaml"
HANDLER_TMP="$ROOT_DIR/tmp/min_handler.erl"
APP="butler_shared"
OUT="$ROOT_DIR/tmp/api.yaml"

rm -rf "$ROOT_DIR/tmp"
mkdir -p "$ROOT_DIR/tmp"
cp "$ROOT_DIR/examples/handlers/min_handler.erl" "$HANDLER_TMP"

bash "$ROOT_DIR/scripts/dev_compile.sh"

echo "[sanity] OpenAPI -> Erlang (dry-run)"
"$ROOT_DIR/scripts/gen_erlang.sh" "$SPEC" "$APP" "$HANDLER_TMP" true false || true

echo "[sanity] OpenAPI -> Erlang (apply with backup)"
"$ROOT_DIR/scripts/gen_erlang.sh" "$SPEC" "$APP" "$HANDLER_TMP" false true

echo "[sanity] Erlang -> OpenAPI"
"$ROOT_DIR/scripts/gen_spec.sh" "$HANDLER_TMP" "$APP" "$OUT"

echo "[sanity] Done. Output spec at $OUT"
