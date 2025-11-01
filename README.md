# erl-openapi

Rebar3 plugin for bidirectional OpenAPI ↔ Erlang handler sync.

## Features

- ✅ **Generate complete handler modules** from OpenAPI 3.x YAML specs
- ✅ **Update existing handlers** with new routes and handle_request clauses
- ✅ **Draft-04 JSON schema artifacts** per `operationId` with proper formatting
- ✅ **Auto-format generated code** using erlfmt
- ✅ **Idempotent**: no duplicate routes or clauses
- ✅ **Dry-run mode** to preview changes
- ✅ **Optional backup** before modifications
- ⚠️ **Spec generation** (Erlang → OpenAPI) is partially implemented

## Quickstart

### 1. Generate a new handler from scratch

If the handler file doesn't exist, it will be created automatically:

```bash
# Compile the plugin first
rebar3 compile

# Generate new handler (creates the file if it doesn't exist)
bash scripts/gen_erlang.sh examples/specs/sample.yaml my_app src/my_handler.erl false false
```

This creates:
- Complete handler module with `routes/0` and `handle_request/3` clauses
- JSON Schema files in `apps/my_app/priv/json_schemas/`
- Auto-formatted Erlang code

### 2. Update an existing handler

If the handler exists, it updates it with new routes:

```bash
# Dry-run first to see changes
bash scripts/gen_erlang.sh path/to/updated_spec.yaml my_app src/my_handler.erl true false

# Apply changes with backup
bash scripts/gen_erlang.sh path/to/updated_spec.yaml my_app src/my_handler.erl false true
```

### 3. Run the sanity test

```bash
bash scripts/sanity_check.sh
```

## Command Reference

### Using scripts (recommended for testing)

```bash
# gen_erlang.sh <spec_path> <app_name> <handler_path> <dry_run> <backup>
bash scripts/gen_erlang.sh examples/specs/sample.yaml my_app src/handler.erl false false

# Arguments:
#   spec_path    - Path to OpenAPI YAML file
#   app_name     - Application name for schema placement
#   handler_path - Path to handler file (created if doesn't exist)
#   dry_run      - true|false (preview changes without writing)
#   backup       - true|false (create .bak files)
```

### Using rebar3 plugin directly

```bash
# From within your Erlang project that includes this plugin
rebar3 openapi gen erlang \
  --spec path/to/openapi.yaml \
  --app your_app \
  --handler path/to/handler.erl \
  --backup
```

## Flags

- `--spec <path>`: Path to OpenAPI YAML file (required)
- `--app <name>`: Application name for schema placement (required)
- `--handler <path>`: Path to handler file (required, created if doesn't exist)
- `--dry-run`: Preview changes without writing files
- `--backup`: Create `.bak` files before modifying
- `--format yaml|json`: Output format for spec generation (default: yaml)

## Docs
- See `docs/API.md` and `docs/ARCHITECTURE.md`.

## License
Apache-2.0. See `LICENSE`.
