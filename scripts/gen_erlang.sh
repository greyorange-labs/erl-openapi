#!/usr/bin/env bash
set -euo pipefail

SPEC=${1:-examples/specs/sample.yaml}
APP=${2:-butler_shared}
HANDLER=${3:-examples/handlers/min_handler.erl}
EXTRA_FLAGS=${4:-}

echo "[gen_erlang] compiling..."
rebar3 compile >/dev/null

echo "[gen_erlang] running: rebar3 openapi gen erlang --spec $SPEC --app $APP --handler $HANDLER $EXTRA_FLAGS"
rebar3 openapi gen erlang --spec "$SPEC" --app "$APP" --handler "$HANDLER" $EXTRA_FLAGS
