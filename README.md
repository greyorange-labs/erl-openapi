# erl-openapi

Rebar3 plugin for bidirectional OpenAPI ↔ Erlang handler sync.

- Generate Erlang routes and `handle_request/3` stubs from OpenAPI 3.x (YAML)
- Generate OpenAPI specs from existing Erlang handlers
- Draft-04 JSON schema artifacts per `operationId`
- Idempotent, with `--dry-run` and optional `--backup`

## Quickstart

```bash
# From repo root
rebar3 compile

# OpenAPI → Erlang (dry-run)
rebar3 openapi gen erlang --spec examples/specs/sample.yaml \
  --app butler_shared \
  --handler examples/handlers/min_handler.erl \
  --dry-run

# Apply with backup
rebar3 openapi gen erlang --spec examples/specs/sample.yaml \
  --app butler_shared \
  --handler examples/handlers/min_handler.erl \
  --backup

# Erlang → OpenAPI
rebar3 openapi gen spec --handler examples/handlers/min_handler.erl \
  --app butler_shared \
  --output docs/api.yaml
```

### Run the sanity script
```bash
bash scripts/sanity_check.sh
```

## Use with a real handler
Replace paths accordingly:
```bash
rebar3 openapi gen erlang \
  --spec /ABS/PATH/TO/your.yaml \
  --app butler_shared \
  --handler /Users/amar.c/workspace/gm_core/butler_server_develop/apps/butler_shared/src/interfaces/in/gm_common_http_handler.erl \
  --backup
```

## Flags
- `--dry-run`: print diffs, do not write
- `--backup`: create `.bak` before modifying handler
- `--format`: `yaml|json` (for spec generation)

## Docs
- See `docs/API.md` and `docs/ARCHITECTURE.md`.

## License
Apache-2.0. See `LICENSE`.
