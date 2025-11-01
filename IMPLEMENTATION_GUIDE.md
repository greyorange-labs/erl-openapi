# Implementation Guide: Bi-directional Sync

## Quick Summary

This guide shows you **exactly** how to structure your Erlang handlers and JSON schemas to enable bi-directional sync between OpenAPI specs and Erlang code.

---

## Two Approaches

### Approach 1: Enhanced Schema Files (Recommended)

**Pros:**
- ✅ All metadata in structured JSON (easy to parse)
- ✅ No need to parse Erlang comments
- ✅ Schemas used for both validation and documentation
- ✅ Easy to edit with tools/IDEs

**Cons:**
- ⚠️ Schema files can get large
- ⚠️ Need to keep schemas in sync with handler

**Best for:** Production systems, team collaboration, automated tooling

### Approach 2: Annotated Handler Comments

**Pros:**
- ✅ Documentation lives with code
- ✅ Smaller schema files (just schemas, no metadata)
- ✅ Better for code reviews

**Cons:**
- ⚠️ Need to parse Erlang comments
- ⚠️ Comments can drift from implementation

**Best for:** Smaller projects, single maintainer, documentation-as-code

---

## Recommended: Enhanced Schema Files

Use **full operation definitions** in JSON schema files, keep handler minimal.

### File Structure

```
apps/butler_shared/
├── src/interfaces/in/
│   └── gm_common_http_handler.erl          # Minimal handler
└── priv/json_schemas/
    ├── _openapi_metadata.json              # Global metadata
    ├── DISABLE-DEBUG-LOGGING.json          # Full operation
    └── ENABLE-DEBUG-LOGGING.json           # Full operation
```

### Handler (Minimal - Just Implementation)

```erlang
-module(gm_common_http_handler).
-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/butler_shared/v1/logging/debug/disable",
            allowed_methods => #{
                <<"POST">> => #{
                    operation_id => 'DISABLE-DEBUG-LOGGING',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        },
        #{
            path => "/api/butler_shared/v1/logging/debug/enable",
            allowed_methods => #{
                <<"POST">> => #{
                    operation_id => 'ENABLE-DEBUG-LOGGING',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        }
    ].

handle_request('DISABLE-DEBUG-LOGGING', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% Business logic
    Reason = maps:get(<<"reason">>, ReqBody),
    Source = maps:get(<<"source">>, ReqBody, <<"manual">>),

    %% Call controller
    {Code, RespBody} = logging_controller:disable_debug(Reason, Source),
    {Code, RespBody};

handle_request('ENABLE-DEBUG-LOGGING', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% Business logic
    Ttl = maps:get(<<"ttl">>, ReqBody, undefined),
    Reason = maps:get(<<"reason">>, ReqBody, undefined),

    %% Call controller
    {Code, RespBody} = logging_controller:enable_debug(Ttl, Reason),
    {Code, RespBody};

handle_request(OperationId, _Req, _Context) ->
    {501, #{error => <<"not_implemented">>, operation => OperationId}}.
```

### Schema File (Complete Operation Definition)

`priv/json_schemas/DISABLE-DEBUG-LOGGING.json`:

```json
{
  "operation_id": "DISABLE-DEBUG-LOGGING",
  "path": "/api/butler_shared/v1/logging/debug/disable",
  "method": "POST",
  "summary": "Disable debug logging",
  "description": "Disables debug logging and returns system to normal logging levels.",
  "tags": ["Debug Logging"],
  "requestBody": {
    "required": true,
    "content": {
      "application/json": {
        "schema": {
          "type": "object",
          "required": ["reason"],
          "properties": {
            "reason": {
              "type": "string",
              "description": "Reason for disabling",
              "minLength": 10,
              "maxLength": 500,
              "example": "Troubleshooting completed"
            },
            "source": {
              "type": "string",
              "description": "Source system",
              "example": "monitoring-dashboard"
            }
          }
        }
      }
    }
  },
  "responses": {
    "200": {
      "description": "Successfully disabled",
      "content": {
        "application/json": {
          "schema": {
            "type": "object",
            "required": ["status", "timestamp"],
            "properties": {
              "status": {
                "type": "string",
                "enum": ["success", "already_disabled"]
              },
              "message": {
                "type": "string"
              },
              "timestamp": {
                "type": "string",
                "format": "date-time"
              }
            }
          }
        }
      }
    },
    "400": {
      "description": "Invalid request",
      "content": {
        "application/json": {
          "schema": {
            "$ref": "#/components/schemas/Error"
          }
        }
      }
    }
  }
}
```

### Global Metadata File

`priv/json_schemas/_openapi_metadata.json`:

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "Butler Shared API",
    "version": "1.0.0",
    "description": "API for Butler system operations",
    "contact": {
      "name": "Platform Team",
      "email": "platform@example.com"
    }
  },
  "servers": [
    {
      "url": "https://api.production.example.com",
      "description": "Production"
    }
  ],
  "security": [
    {
      "bearerAuth": []
    }
  ],
  "components": {
    "securitySchemes": {
      "bearerAuth": {
        "type": "http",
        "scheme": "bearer",
        "bearerFormat": "JWT"
      }
    },
    "schemas": {
      "Error": {
        "type": "object",
        "required": ["error", "message"],
        "properties": {
          "error": {
            "type": "string"
          },
          "message": {
            "type": "string"
          }
        }
      }
    }
  }
}
```

---

## Generation Algorithm

### OpenAPI → Erlang (Current)

```
Input: api_spec.yaml

Steps:
1. Parse YAML → OpenAPI map
2. Extract paths and operations
3. For each operation:
   - Extract path, method, operationId
   - Extract request schema
   - Extract response schemas
   - Create JSON file: <operationId>.json with FULL operation definition
4. Update/create handler:
   - Add route to routes/0 (if not exists)
   - Add handle_request/3 clause (if not exists)
5. Format with erlfmt

Output:
- handler.erl (updated)
- <operationId>.json files (created/updated)
```

### Erlang → OpenAPI (To Implement)

```
Input: handler.erl + JSON schema files

Steps:
1. Read _openapi_metadata.json → base OpenAPI structure
2. Parse handler.erl:
   - Extract routes/0 → list of {path, method, operationId}
3. For each operationId:
   - Read <operationId>.json
   - Extract: summary, description, tags, requestBody, responses
   - Build OpenAPI operation object
4. Assemble final spec:
   - info, servers, security from metadata
   - paths from collected operations
   - components from metadata
5. Convert to YAML

Output: api_spec.yaml
```

---

## Practical Example

### Scenario: Add New Operation

**Step 1: Update OpenAPI Spec**

```yaml
# api_spec.yaml - add new operation
paths:
  /api/butler_shared/v1/logging/debug/status:
    get:
      operationId: GET-DEBUG-STATUS
      summary: Get debug logging status
      responses:
        "200":
          description: Current status
          content:
            application/json:
              schema:
                type: object
                properties:
                  enabled:
                    type: boolean
```

**Step 2: Sync to Erlang**

```bash
$ rebar3 openapi gen erlang \
    --spec api_spec.yaml \
    --handler src/gm_common_http_handler.erl
```

**Result:**
- `gm_common_http_handler.erl` gets new route + clause
- `priv/json_schemas/GET-DEBUG-STATUS.json` created
- Handler is formatted with erlfmt

**Step 3: Implement Logic**

```erlang
% In gm_common_http_handler.erl
handle_request('GET-DEBUG-STATUS', _Req, _Context) ->
    Enabled = logging_controller:is_debug_enabled(),
    {200, #{enabled => Enabled}}.
```

**Step 4: Verify Sync**

```bash
$ rebar3 openapi gen spec \
    --handler src/gm_common_http_handler.erl \
    --output api_spec_check.yaml

$ diff api_spec.yaml api_spec_check.yaml
# Should be identical (or minimal differences)
```

---

## Best Practices

### ✅ DO

1. **Keep schemas as single source of truth**
   - Put all metadata in JSON schemas
   - Let handler focus on business logic

2. **Use _openapi_metadata.json for globals**
   - info, servers, security, components
   - Version it with your app

3. **Validate at runtime**
   - Use same JSON schemas for request validation
   - Ensures docs match behavior

4. **Run sync regularly**
   - Before releases
   - After API changes
   - In CI/CD pipeline

5. **Keep operationId stable**
   - Use as permanent identifier
   - Don't change without versioning

### ❌ DON'T

1. **Don't duplicate metadata**
   - Pick ONE place (schemas recommended)
   - Don't maintain in multiple places

2. **Don't edit generated code manually**
   - If you must, use "TODO" comments only
   - Re-generation will preserve your logic

3. **Don't skip validation**
   - Always test generated handlers
   - Verify requests match schemas

4. **Don't ignore drift**
   - If spec and code diverge, fix immediately
   - Use diff to detect drift

---

## Testing Your Implementation

### Test OpenAPI → Erlang

```bash
# 1. Generate from spec
rebar3 openapi gen erlang --spec api.yaml --handler handler.erl

# 2. Compile
rebar3 compile

# 3. Check formatting
rebar3 fmt --check handler.erl

# 4. Run tests
rebar3 eunit
```

### Test Erlang → OpenAPI

```bash
# 1. Generate spec from code
rebar3 openapi gen spec --handler handler.erl --output generated.yaml

# 2. Validate generated spec
npm install -g @apidevtools/swagger-cli
swagger-cli validate generated.yaml

# 3. Compare with original
diff original.yaml generated.yaml
```

### Test Round-Trip

```bash
# 1. Start with OpenAPI
cp original.yaml step1.yaml

# 2. Generate Erlang
rebar3 openapi gen erlang --spec step1.yaml --handler handler.erl

# 3. Generate OpenAPI back
rebar3 openapi gen spec --handler handler.erl --output step2.yaml

# 4. Compare (should be identical)
diff step1.yaml step2.yaml
```

---

## Next Steps

1. **Try the examples:**
   ```bash
   cd examples/
   ls -la handlers/annotated_handler.erl
   ls -la schemas/
   ```

2. **Read the full strategy:**
   ```bash
   cat BIDIRECTIONAL_SYNC.md
   ```

3. **Test with your API:**
   - Start with one operation
   - Test both directions
   - Expand to full API

4. **Implement Erlang → OpenAPI:**
   - See BIDIRECTIONAL_SYNC.md section 9
   - Modules to implement listed there

