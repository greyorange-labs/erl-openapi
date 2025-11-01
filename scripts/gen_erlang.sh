#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
SPEC=${1:-$ROOT_DIR/examples/specs/sample.yaml}
APP=${2:-butler_shared}
HANDLER=${3:-$ROOT_DIR/examples/handlers/min_handler.erl}
DRY_RUN=${4:-true}
BACKUP=${5:-false}

bash "$ROOT_DIR/scripts/dev_compile.sh"

# Add all library paths including our own compiled modules
PA_ARGS=""
for lib in "$ROOT_DIR/_build/default/lib"/*; do
  if [ -d "$lib/ebin" ]; then
    PA_ARGS="$PA_ARGS -pa $lib/ebin"
  fi
done

erl -noshell $PA_ARGS -eval \
  "case cli_runner:gen_erlang(\"$SPEC\", \"$APP\", \"$HANDLER\", $DRY_RUN, $BACKUP) of ok -> ok; {error,E} -> io:format(\"ERROR: ~p~n\", [E]), halt(1) end, init:stop()."
