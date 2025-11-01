#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)

# Use rebar3 to compile properly
cd "$ROOT_DIR"
rebar3 compile >/dev/null 2>&1

echo "Compiled modules via rebar3"
