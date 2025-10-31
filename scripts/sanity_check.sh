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

echo "[sanity] rebar3 compile"
rebar3 compile >/dev/null

echo "[sanity] Dry run gen erlang"
rebar3 openapi gen erlang --spec "$SPEC" --app "$APP" --handler "$HANDLER_TMP" --dry-run

echo "[sanity] Apply gen erlang with backup"
rebar3 openapi gen erlang --spec "$SPEC" --app "$APP" --handler "$HANDLER_TMP" --backup

echo "[sanity] Generate spec from handler"
rebar3 openapi gen spec --handler "$HANDLER_TMP" --app "$APP" --output "$OUT"

echo "[sanity] Done. Outputs:"
ls -la "$ROOT_DIR/tmp"
