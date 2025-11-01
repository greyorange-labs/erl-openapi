# API

## Commands

### OpenAPI → Erlang
```
rebar3 openapi gen erlang \
  --spec <path-to-openapi.yaml> \
  --app <erlang_app_name> \
  --handler <path-to-handler.erl> \
  [--dry-run] [--backup]
```
- Writes Draft-04 schemas to `apps/<app>/priv/json_schemas/`
- Appends route entries to `routes/0` (with placement comment)
- Inserts `handle_request/3` stubs before catch-all in requested style

Stub format:
```
handle_request('NEW-OPS-ID', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = bsh_logging_http_controller:disable_debug(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>}
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
