# Architecture

## Overview
- Provider: `rebar3_openapi_prv`
- YAML loader/validator: `openapi_yaml_loader`, `openapi_validator`
- Schema handling: `schema_draft04_converter`, `schema_writer`
- Handler parsing/updating: `handler_parser`, `handler_routes_updater`, `handler_clauses_updater`
- Spec generation: `spec_generator`
- Utilities: `diff_preview`, `backup_manager`, `logging`

## Flow: OpenAPI → Erlang
```mermaid
flowchart TD
  A[OpenAPI YAML] --> B[yamerl parse]
  B --> C[validate structure]
  C --> D[list operations]
  D --> E[write Draft-04 schemas]
  D --> F[update routes/0]
  D --> G[insert handle_request/3 stubs]
  F & G --> H[diff + backup handling]
```

## Flow: Erlang → OpenAPI
```mermaid
flowchart TD
  A[handler.erl] --> B[parse routes/0]
  A --> C[scan handle_request/3]
  B & C --> D[filter implemented ops]
  D --> E[build OpenAPI 3.0.3]
  E --> F[write YAML/JSON]
```

## Notes
- YAML only for input (3.0.x prioritized; 3.1 best-effort)
- No operationId normalization; exact usage as in handlers
- Dry-run prints unified diff; backups optional
