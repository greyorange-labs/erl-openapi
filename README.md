# rebar3_openapi

Rebar3 plugin for bidirectional OpenAPI 3.x ↔ Erlang handler synchronization.

## Features

- ✅ **Bidirectional sync**: OpenAPI ↔ Erlang
- ✅ **Generate handlers** from OpenAPI specs with JSON schemas
- ✅ **Generate OpenAPI specs** from handlers with proper ordering
- ✅ **Versioned schemas**: Auto-detect changes, create versioned files
- ✅ **Component & inline schemas**: Supports both patterns
- ✅ **camelCase enforcement**: Validates operationId naming
- ✅ **Idempotent updates**: No duplicates, safe re-runs
- ✅ **Auto-format**: erlfmt integration

## Installation

### Add to Your Project

Add to your `rebar.config`:

```erlang
{deps, [
    {rebar3_openapi, {git, "https://github.com/greyorange-labs/erl-openapi", {branch, "main"}}}
]}.

{plugins, [
    {rebar3_openapi, {git, "https://github.com/greyorange-labs/erl-openapi", {branch, "main"}}}
]}.
```

### Verify Installation

```bash
rebar3 update
rebar3 help openapi_gen_erlang
rebar3 help openapi_gen_spec
```

## Quick Start

### 1. Generate Handler from OpenAPI Spec

Create a new handler or update an existing one:

```bash
rebar3 openapi_gen_erlang \
  --spec path/to/api.yaml \
  --handler src/my_handler.erl \
  --app my_app
```

This will:
- Create or update `src/my_handler.erl` with routes and function stubs
- Generate JSON schema files in `apps/my_app/priv/json_schemas/`
- Auto-format the generated code with erlfmt

### 2. Generate OpenAPI Spec from Handler

Create an OpenAPI spec from your implemented handler:

```bash
rebar3 openapi_gen_spec \
  --handler src/my_handler.erl \
  --app my_app \
  --output docs/api.yaml
```

This will:
- Parse routes from `routes/0` function
- Read JSON schemas from `apps/my_app/priv/json_schemas/`
- Generate complete OpenAPI 3.x YAML specification

## Commands

### openapi_gen_erlang

Generate or update Erlang handler from OpenAPI specification.

```bash
rebar3 openapi_gen_erlang --spec <path> --handler <path> --app <name> [options]
```

**Required Arguments:**
- `--spec <path>` - Path to OpenAPI YAML file
- `--handler <path>` - Path to handler file (created if doesn't exist)
- `--app <name>` - Application name for schema placement

**Optional Arguments:**
- `--dry-run` - Preview changes without writing files
- `--backup` - Create `.bak` backup files before modifying

**Examples:**

```bash
# Generate new handler
rebar3 openapi_gen_erlang \
  --spec specs/users_api.yaml \
  --handler src/users_handler.erl \
  --app my_app

# Update existing handler with dry-run
rebar3 openapi_gen_erlang \
  --spec specs/users_api_v2.yaml \
  --handler src/users_handler.erl \
  --app my_app \
  --dry-run

# Update with backup
rebar3 openapi_gen_erlang \
  --spec specs/users_api_v2.yaml \
  --handler src/users_handler.erl \
  --app my_app \
  --backup
```

### openapi_gen_spec

Generate OpenAPI specification from Erlang handler and JSON schemas.

```bash
rebar3 openapi_gen_spec --handler <path> --app <name> --output <path> [options]
```

**Required Arguments:**
- `--handler <path>` - Path to handler file to parse
- `--app <name>` - Application name for schema lookup
- `--output <path>` - Output path for generated OpenAPI spec

**Optional Arguments:**
- `--format yaml|json` - Output format (default: yaml)

**Examples:**

```bash
# Generate YAML spec
rebar3 openapi_gen_spec \
  --handler src/users_handler.erl \
  --app my_app \
  --output docs/users_api.yaml

# Generate JSON spec
rebar3 openapi_gen_spec \
  --handler src/users_handler.erl \
  --app my_app \
  --output docs/users_api.json \
  --format json
```

## operationId Naming Convention

**All operationIds must be in camelCase format:**
- ✅ Start with lowercase letter (a-z)
- ✅ Only letters (a-zA-Z) and numbers (0-9)
- ❌ No hyphens, underscores, or special characters

**Valid Examples:**
- `getUsers`
- `createUser`
- `getUserById`
- `updateOrder`
- `deleteItem`

**Invalid Examples:**
- ❌ `get-users` (kebab-case)
- ❌ `get_users` (snake_case)
- ❌ `GetUsers` (PascalCase)
- ❌ `GET-USERS` (SCREAMING-CAPS)

The tool will validate operationIds and provide helpful error messages if they don't follow the convention.

## File Structure

### Generated Handler Module

```erlang
-module(my_handler).
-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/v1/users",
            allowed_methods => #{
                <<"get">> => #{operation_id => getUserList}
            }
        },
        #{
            path => "/api/v1/users",
            allowed_methods => #{
                <<"post">> => #{
                    operation_id => createUser,
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        }
    ].

handle_request(getUserList, _Req, _Context) ->
    %% TODO: Implement business logic
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};

handle_request(createUser, #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Implement business logic
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};

handle_request(OperationId, _Req, Context) ->
    RespBody = #{message => <<"Not implemented">>},
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.
```

### JSON Schema Files

**Location:** `apps/<app_name>/priv/json_schemas/`

**Directory Structure:**
```
apps/my_app/priv/json_schemas/
├── _openapi_metadata.json          # Global metadata
├── operations/                      # Operation definitions
│   ├── createUser.json
│   └── getUserById.json
└── components/                      # Reusable schemas
    └── schemas/
        ├── User.json
        ├── User_v2.json            # Versioned schema
        └── ErrorResponse.json
```

**Operation files** (in `operations/`):

**`operations/createUser.json`:**
```json
{
  "operation_id": "createUser",
  "path": "/api/v1/users",
  "method": "POST",
  "summary": "Create a new user",
  "requestBody": {
    "content": {
      "application/json": {
        "schema": {
          "$ref": "#/components/schemas/UserInput"
        }
      }
    },
    "required": true
  },
  "responses": {
    "201": {
      "description": "User created",
      "content": {
        "application/json": {
          "schema": {
            "$ref": "#/components/schemas/User"
          }
        }
      }
    }
  }
}
```

**`components/schemas/User.json`:**
```json
{
  "type": "object",
  "properties": {
    "id": {"type": "string"},
    "name": {"type": "string"},
    "email": {"type": "string"}
  }
}
```

**`_openapi_metadata.json`:**
```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "My API",
    "version": "1.0.0"
  },
  "servers": [{"url": "https://api.example.com"}]
}
```

### Schema Versioning

When schemas change, versioned files are created automatically:

```
⚠️  Schema conflict detected for 'User'
   Existing: apps/my_app/priv/json_schemas/components/schemas/User.json
   New version: apps/my_app/priv/json_schemas/components/schemas/User_v2.json
   → Review both schemas and update $ref paths if needed.
```

**Features:**
- Content-based comparison (only creates new version if different)
- Sequential versioning (`User.json`, `User_v2.json`, `User_v3.json`)
- Automatic $ref path updates in operation files
- Orphaned file detection

## Workflow

### Typical Development Flow

1. **Start with OpenAPI spec** - Design your API
2. **Generate handler** - Create Erlang code structure
3. **Implement business logic** - Fill in the TODOs
4. **Update spec** - Add new routes to OpenAPI
5. **Update handler** - Regenerate to add new routes
6. **Generate docs** - Create updated OpenAPI spec from code

### Update Existing API

```bash
# 1. Dry run to see what will change
rebar3 openapi_gen_erlang \
  --spec specs/api_v2.yaml \
  --handler src/my_handler.erl \
  --app my_app \
  --dry-run

# 2. Review the changes shown

# 3. Apply with backup
rebar3 openapi_gen_erlang \
  --spec specs/api_v2.yaml \
  --handler src/my_handler.erl \
  --app my_app \
  --backup

# 4. Implement new operations
# Edit src/my_handler.erl and add business logic

# 5. Generate updated spec
rebar3 openapi_gen_spec \
  --handler src/my_handler.erl \
  --app my_app \
  --output docs/api_v2.yaml
```

## Idempotency

Running the same command multiple times produces the same result:
- No duplicate routes in `routes/0`
- No duplicate function clauses in `handle_request/3`
- Existing implementations are preserved
- Safe to run repeatedly

## Error Handling

The tool provides clear, actionable error messages:

```
ERROR: Invalid operationId 'get-users' for GET /api/users
       operationId must be in camelCase format
       - Must start with lowercase letter (a-z)
       - Can only contain letters (a-zA-Z) and numbers (0-9)
       - No hyphens, underscores, or spaces allowed
       Examples: createUser, getUserById, updateOrder, deleteItem
```

## Examples

See `examples/` directory for:
- `examples/specs/sample.yaml` - Example OpenAPI specification
- `examples/handlers/min_handler.erl` - Minimal handler example
- `examples/schemas/` - Example JSON schema files

## Troubleshooting

### Handler not formatting correctly?

Ensure erlfmt is installed:
```bash
rebar3 plugins list | grep erlfmt
```

Add to your `rebar.config` if missing:
```erlang
{plugins, [erlfmt]}.
```

### Schema files not generated?

Check that the app directory exists:
```bash
ls -la apps/my_app/priv/json_schemas/
```

Create it if needed:
```bash
mkdir -p apps/my_app/priv/json_schemas/
```

### OpenAPI validation errors?

Validate your spec with standard tools:
```bash
npm install -g @apidevtools/swagger-cli
swagger-cli validate api.yaml
```

## Requirements

- Erlang/OTP 27.x or later
- Rebar3 3.25.x or later
- erlfmt plugin (for code formatting)

## Dependencies

- `jsx` - JSON encoding/decoding
- `yamerl` - YAML parsing
- `erlfmt` - Erlang code formatting

## Contributing

See `CONTRIBUTING.md` for guidelines.

## License

Apache-2.0 - See `LICENSE` file.

## Support

- **Issues**: https://github.com/greyorange-labs/erl-openapi/issues
- **Examples**: See `examples/` directory
- **Architecture**: See `docs/ARCHITECTURE.md`
