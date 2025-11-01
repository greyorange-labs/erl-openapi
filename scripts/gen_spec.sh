#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
HANDLER=${1:-$ROOT_DIR/examples/handlers/min_handler.erl}
APP=${2:-butler_shared}
OUTPUT=${3:-$ROOT_DIR/docs/api.yaml}

mkdir -p "$(dirname "$OUTPUT")"

bash "$ROOT_DIR/scripts/dev_compile.sh"

# Add all library paths including our own compiled modules
PA_ARGS=""
for lib in "$ROOT_DIR/_build/default/lib"/*; do
  if [ -d "$lib/ebin" ]; then
    PA_ARGS="$PA_ARGS -pa $lib/ebin"
  fi
done

erl -noshell $PA_ARGS -eval \
  "case cli_runner:gen_spec(\"$HANDLER\", \"$APP\", \"$OUTPUT\") of ok -> ok; {error,E} -> io:format(\"ERROR: ~p~n\", [E]), halt(1) end, init:stop()."
