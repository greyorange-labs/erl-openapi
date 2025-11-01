# API

## Commands

### OpenAPI → Erlang

Generates or updates Erlang handler modules from OpenAPI specifications.

```bash
rebar3 openapi gen erlang \
  --spec <path-to-openapi.yaml> \
  --app <erlang_app_name> \
  --handler <path-to-handler.erl> \
  [--dry-run] [--backup]
```

**Behavior:**
- **If handler doesn't exist**: Creates complete handler module from scratch
- **If handler exists**: Updates it with new routes and clauses (idempotent)

**What it generates:**
1. **Handler module** with:
   - `routes/0` function with route entries
   - `handle_request/3` clauses for each operationId
   - Catch-all clause
   - Auto-formatted using erlfmt

2. **JSON Schema files** in `apps/<app>/priv/json_schemas/`:
   - One JSON file per operationId
   - Draft-04 format
   - Properly formatted with indentation

**Generated stub format:**
```erlang
handle_request('NEW-OPS-ID', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_new-ops-id(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
```

### Erlang → OpenAPI
```
rebar3 openapi gen spec \
  --handler <path-to-handler.erl> \
  --app <erlang_app_name> \
  --output <path-to-output.yaml> \
  [--format yaml|json]
```
- Extracts implemented `operationId`s and `routes/0`
- Emits OpenAPI 3.0.3 with placeholders when schemas are missing

## Options
- `--dry-run`: show unified diff only
- `--backup`: write `.bak` before changes
- `--format`: output format for spec generation (default `yaml`)

## Exit codes
- `0` on success
- `>0` with a message and suggestion when validation or file IO fails

## Examples
See `README.md` and `scripts/`.
