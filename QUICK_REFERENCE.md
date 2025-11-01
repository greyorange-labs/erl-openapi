# Quick Reference: Bi-directional Sync

## 📁 File Templates

### 1. Handler Module (Minimal Implementation)

```erlang
-module(my_handler).
-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/v1/resource",
            allowed_methods => #{
                <<"POST">> => #{
                    operation_id => 'CREATE-RESOURCE',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        }
    ].

handle_request('CREATE-RESOURCE', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% Your business logic here
    Name = maps:get(<<"name">>, ReqBody),
    {Code, RespBody} = my_controller:create(Name),
    {Code, RespBody};

handle_request(OperationId, _Req, _Context) ->
    {501, #{error => <<"not_implemented">>, operation => OperationId}}.
```

---

### 2. Enhanced Schema File (Full Operation)

**File:** `priv/json_schemas/CREATE-RESOURCE.json`

```json
{
  "operation_id": "CREATE-RESOURCE",
  "path": "/api/v1/resource",
  "method": "POST",
  "summary": "Create a new resource",
  "description": "Creates a resource with the given name",
  "tags": ["Resources"],
  "requestBody": {
    "required": true,
    "content": {
      "application/json": {
        "schema": {
          "type": "object",
          "required": ["name"],
          "properties": {
            "name": {
              "type": "string",
              "description": "Name of the resource",
              "minLength": 3,
              "maxLength": 100,
              "example": "My Resource"
            }
          }
        }
      }
    }
  },
  "responses": {
    "201": {
      "description": "Resource created successfully",
      "content": {
        "application/json": {
          "schema": {
            "type": "object",
            "properties": {
              "id": {
                "type": "string",
                "example": "res-12345"
              },
              "name": {
                "type": "string",
                "example": "My Resource"
              }
            }
          }
        }
      }
    },
    "400": {
      "description": "Invalid input",
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

---

### 3. Global Metadata File

**File:** `priv/json_schemas/_openapi_metadata.json`

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "My API",
    "version": "1.0.0",
    "description": "API description here",
    "contact": {
      "name": "API Team",
      "email": "api@example.com"
    }
  },
  "servers": [
    {
      "url": "https://api.example.com",
      "description": "Production"
    },
    {
      "url": "http://localhost:8080",
      "description": "Local"
    }
  ],
  "security": [
    {
      "bearerAuth": []
    }
  ],
  "tags": [
    {
      "name": "Resources",
      "description": "Resource management"
    }
  ],
  "components": {
    "securitySchemes": {
      "bearerAuth": {
        "type": "http",
        "scheme": "bearer",
        "bearerFormat": "JWT",
        "description": "JWT token authentication"
      }
    },
    "schemas": {
      "Error": {
        "type": "object",
        "required": ["error", "message"],
        "properties": {
          "error": {
            "type": "string",
            "description": "Error code",
            "example": "validation_error"
          },
          "message": {
            "type": "string",
            "description": "Error message",
            "example": "Invalid input"
          },
          "trace_id": {
            "type": "string",
            "description": "Trace ID for debugging",
            "example": "trace-abc-123"
          }
        }
      }
    }
  }
}
```

---

## 🔄 Common Operations

### Generate Erlang from OpenAPI

```bash
# Basic
rebar3 openapi gen erlang \
  --spec api.yaml \
  --handler src/my_handler.erl

# With app name
rebar3 openapi gen erlang \
  --spec api.yaml \
  --app my_app \
  --handler src/my_handler.erl

# Dry run (preview changes)
rebar3 openapi gen erlang \
  --spec api.yaml \
  --handler src/my_handler.erl \
  --dry-run

# With backup
rebar3 openapi gen erlang \
  --spec api.yaml \
  --handler src/my_handler.erl \
  --backup
```

### Generate OpenAPI from Erlang (Future)

```bash
# Basic
rebar3 openapi gen spec \
  --handler src/my_handler.erl \
  --output api.yaml

# With schemas directory
rebar3 openapi gen spec \
  --handler src/my_handler.erl \
  --schemas priv/json_schemas \
  --output api.yaml
```

---

## 📝 Schema File Checklist

When creating a schema file manually, include:

- [ ] `operation_id` (matches handler)
- [ ] `path` (API endpoint)
- [ ] `method` (GET, POST, etc.)
- [ ] `summary` (one-line description)
- [ ] `description` (detailed explanation)
- [ ] `tags` (for grouping)
- [ ] `requestBody` (with schema and examples)
- [ ] `responses` (at least 200, 400, 401, 500)
- [ ] Field descriptions and constraints
- [ ] Examples for requests and responses

---

## 🎯 Response Status Codes

Common codes to include in your schemas:

| Code | Description         | When to Use                             |
| ---- | ------------------- | --------------------------------------- |
| 200  | OK                  | Successful GET/PUT/DELETE               |
| 201  | Created             | Successful POST (created resource)      |
| 204  | No Content          | Successful DELETE (no body)             |
| 400  | Bad Request         | Invalid input/validation error          |
| 401  | Unauthorized        | Missing/invalid auth token              |
| 403  | Forbidden           | Valid auth but insufficient permissions |
| 404  | Not Found           | Resource doesn't exist                  |
| 409  | Conflict            | Duplicate/constraint violation          |
| 422  | Unprocessable       | Valid format but semantic error         |
| 429  | Too Many Requests   | Rate limit exceeded                     |
| 500  | Internal Error      | Unexpected server error                 |
| 501  | Not Implemented     | Operation not yet implemented           |
| 503  | Service Unavailable | System overloaded/maintenance           |

---

## 🔍 Validation

### JSON Schema Constraints

```json
{
  "type": "string",
  "minLength": 10,           // Min characters
  "maxLength": 100,          // Max characters
  "pattern": "^[A-Z]",       // Regex pattern
  "format": "email",         // Standard format
  "enum": ["a", "b", "c"]    // Allowed values
}

{
  "type": "integer",
  "minimum": 0,              // Min value (inclusive)
  "maximum": 100,            // Max value (inclusive)
  "exclusiveMinimum": 0,     // Min value (exclusive)
  "multipleOf": 5            // Must be multiple of
}

{
  "type": "array",
  "minItems": 1,             // Min array length
  "maxItems": 10,            // Max array length
  "uniqueItems": true        // All items unique
}

{
  "type": "object",
  "required": ["field1"],    // Required fields
  "additionalProperties": false  // No extra fields
}
```

---

## 🛠️ Troubleshooting

### Handler not updating?

```bash
# Check if route already exists
grep "operation_id => 'YOUR-OP-ID'" src/handler.erl

# Use --force to overwrite (future feature)
# Or manually remove the route and re-run
```

### Schema file not generated?

```bash
# Check app directory exists
ls -la apps/your_app/priv/json_schemas/

# Check operation was parsed
# Look for "Processing operation: YOUR-OP-ID" in output
```

### Format looks wrong?

```bash
# Manually format
rebar3 fmt -w src/handler.erl

# Check erlfmt is installed
rebar3 plugins list | grep erlfmt
```

### Can't parse YAML?

```bash
# Validate YAML syntax
npm install -g @apidevtools/swagger-cli
swagger-cli validate api.yaml

# Check YAML is valid UTF-8
file api.yaml
```

---

## 📚 More Information

- **Full Strategy:** `BIDIRECTIONAL_SYNC.md`
- **Implementation Guide:** `IMPLEMENTATION_GUIDE.md`
- **Examples:** `examples/handlers/` and `examples/schemas/`
- **Requirements:** `requirement.md`

---

## 💡 Tips

1. **Start small:** One operation at a time
2. **Use examples:** Copy from `examples/schemas/`
3. **Validate often:** Run both directions frequently
4. **Keep it DRY:** Use `$ref` for common schemas
5. **Version your metadata:** Commit `_openapi_metadata.json`
6. **Test in CI/CD:** Detect drift automatically

