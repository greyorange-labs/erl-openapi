#!/usr/bin/env bash
set -euo pipefail

HANDLER=${1:-examples/handlers/min_handler.erl}
APP=${2:-butler_shared}
OUTPUT=${3:-docs/api.yaml}
EXTRA_FLAGS=${4:-}

mkdir -p "$(dirname "$OUTPUT")"

echo "[gen_spec] compiling..."
rebar3 compile >/dev/null

echo "[gen_spec] running: rebar3 openapi gen spec --handler $HANDLER --app $APP --output $OUTPUT $EXTRA_FLAGS"
rebar3 openapi gen spec --handler "$HANDLER" --app "$APP" --output "$OUTPUT" $EXTRA_FLAGS
