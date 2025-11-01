# Bi-directional Sync Strategy

This document explains how to structure Erlang handlers and JSON schemas to enable **bi-directional synchronization** between OpenAPI specs and Erlang code.

## Overview

### OpenAPI → Erlang (Already Implemented)
- Parse OpenAPI YAML spec
- Generate/update Erlang handler modules
- Generate JSON schema files per operation

### Erlang → OpenAPI (To Be Implemented)
- Parse Erlang handler comments/annotations
- Read JSON schema files
- Generate complete OpenAPI YAML spec

---

## File Structure

```
apps/your_app/
├── src/interfaces/in/
│   └── your_handler.erl              # Handler with annotations
└── priv/json_schemas/
    ├── _openapi_metadata.json        # Global OpenAPI metadata
    ├── OPERATION-ID-1.json           # Per-operation full schema
    └── OPERATION-ID-2.json           # Per-operation full schema
```

---

## 1. Annotated Erlang Handler

### Module-Level Annotations (Optional but Recommended)

These provide global OpenAPI metadata:

```erlang
%%% @openapi_info
%%% title: Your API Title
%%% version: 1.0.0
%%% description: Multi-line description of your API
%%% contact_name: Team Name
%%% contact_email: team@example.com

%%% @openapi_servers
%%% - url: https://api.production.example.com | description: Production
%%% - url: http://localhost:8080 | description: Local

%%% @openapi_security
%%% - bearerAuth: []

%%% @openapi_tags
%%% - name: Tag Name | description: Tag description
```

### Route-Level Annotations

Before each route in `routes/0`, add metadata:

```erlang
routes() ->
    [
        %%% @route OPERATION-ID
        %%% summary: Short one-line summary
        %%% description: Detailed description (can be multi-line)
        %%% permissions: required:permissions
        %%% tag: Tag Name
        %%% note: Additional notes (optional)
        #{
            path => "/api/path",
            allowed_methods => #{
                <<"POST">> => #{
                    operation_id => 'OPERATION-ID',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        }
    ].
```

### Function-Level Annotations

Before each `handle_request/3` clause:

```erlang
%%% @operation OPERATION-ID
%%% @http_status 200: Success description
%%% @http_status 400: Bad request description
%%% @http_status 401: Unauthorized description
%%% @http_status 403: Forbidden description
%%% @http_status 500: Server error description
handle_request('OPERATION-ID', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% Implementation
    {Code, RespBody}.
```

---

## 2. Enhanced JSON Schema Files

### Per-Operation Schema Files

Each `<OPERATION-ID>.json` contains the **complete OpenAPI operation definition**:

```json
{
  "operation_id": "OPERATION-ID",
  "path": "/api/path",
  "method": "POST",
  "summary": "Short summary",
  "description": "Detailed description",
  "tags": ["Tag Name"],
  "requestBody": {
    "required": true,
    "content": {
      "application/json": {
        "schema": {
          "type": "object",
          "required": ["field1"],
          "properties": {
            "field1": {
              "type": "string",
              "description": "Field description",
              "minLength": 10,
              "example": "example value"
            }
          }
        },
        "example": {
          "field1": "example value"
        }
      }
    }
  },
  "responses": {
    "200": {
      "description": "Success",
      "content": {
        "application/json": {
          "schema": {
            "type": "object",
            "properties": {
              "status": {
                "type": "string",
                "example": "success"
              }
            }
          },
          "example": {
            "status": "success"
          }
        }
      }
    },
    "400": {
      "description": "Bad request",
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

`_openapi_metadata.json` contains shared OpenAPI information:

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "API Title",
    "version": "1.0.0",
    "description": "API description",
    "contact": {
      "name": "Team Name",
      "email": "team@example.com"
    }
  },
  "servers": [
    {
      "url": "https://api.example.com",
      "description": "Production"
    }
  ],
  "security": [
    {
      "bearerAuth": []
    }
  ],
  "tags": [
    {
      "name": "Tag Name",
      "description": "Tag description"
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
            "type": "string",
            "example": "validation_error"
          },
          "message": {
            "type": "string",
            "example": "Error message"
          }
        }
      }
    }
  }
}
```

---

## 3. Generation Logic

### OpenAPI → Erlang (Current Implementation)

```
1. Parse OpenAPI YAML
2. Extract operations (path + method + operationId)
3. Generate/update handler routes/0 function
4. Generate/update handle_request/3 clauses
5. Write enhanced JSON schema files (full operation definitions)
6. Format with erlfmt
```

### Erlang → OpenAPI (Future Implementation)

```
1. Parse handler module:
   a. Extract module-level @openapi_* annotations
   b. Parse routes/0 to get path/method/operationId mapping
   c. Extract @route annotations for summaries/descriptions
   d. Extract @operation annotations for HTTP status codes

2. Read _openapi_metadata.json for global info

3. For each operationId:
   a. Read <operationId>.json
   b. Merge with annotations from handler
   c. Build complete OpenAPI operation object

4. Assemble final OpenAPI spec:
   - info, servers, security, tags from metadata
   - paths from combined handler + schema data
   - components from metadata

5. Write OpenAPI YAML file
```

---

## 4. Priority of Information Sources

When generating OpenAPI from Erlang, use this priority:

1. **JSON schema files** (most complete, authoritative source)
2. **Handler annotations** (for summaries, descriptions, HTTP codes)
3. **Module-level annotations** (for global metadata)
4. **_openapi_metadata.json** (fallback for global metadata)

---

## 5. Minimal vs Full Documentation

### Minimal Approach (Quick Start)
- Just create basic JSON schema files with request/response schemas
- No annotations in Erlang handler
- Use _openapi_metadata.json for global info
- Generated OpenAPI will be functional but minimal

### Full Approach (Production Ready)
- Enhanced JSON schema files with full operation definitions
- Annotated Erlang handler for better code documentation
- Comprehensive _openapi_metadata.json
- Generated OpenAPI will be complete and well-documented

---

## 6. Benefits of This Approach

### ✅ Single Source of Truth
- JSON schemas contain complete operation metadata
- Changes in schema files sync to both directions

### ✅ Erlang Code Self-Documenting
- Annotations make code easier to understand
- No need to check separate docs

### ✅ Validation
- JSON schemas used for runtime request validation
- Same schemas used to generate documentation

### ✅ Idempotency
- Repeated syncs don't change output
- Safe to run multiple times

### ✅ Gradual Adoption
- Start with minimal schemas
- Add more detail over time
- Backward compatible

---

## 7. Example Workflow

### Developer Flow 1: API-First (Design → Implement)

```bash
# 1. Design API in OpenAPI spec
vim api_spec.yaml

# 2. Generate Erlang handler + schemas
rebar3 openapi gen erlang --spec api_spec.yaml --handler my_handler.erl

# 3. Implement business logic in generated handler
vim src/my_handler.erl

# 4. Test
rebar3 eunit
```

### Developer Flow 2: Code-First (Implement → Document)

```bash
# 1. Implement handler with annotations
vim src/my_handler.erl

# 2. Create/update JSON schemas manually
vim priv/json_schemas/MY-OPERATION.json

# 3. Generate OpenAPI spec from code + schemas
rebar3 openapi gen spec --handler my_handler.erl --output api_spec.yaml

# 4. Publish API docs
cp api_spec.yaml docs/
```

### Developer Flow 3: Sync (Keep in Sync)

```bash
# 1. Update OpenAPI spec
vim api_spec.yaml

# 2. Sync to Erlang (updates handler + schemas)
rebar3 openapi gen erlang --spec api_spec.yaml --handler my_handler.erl

# 3. Regenerate spec to verify (should be identical)
rebar3 openapi gen spec --handler my_handler.erl --output api_spec_check.yaml

# 4. Compare
diff api_spec.yaml api_spec_check.yaml
```

---

## 8. Migration Path

### Existing Handlers Without Annotations

```bash
# 1. Generate schemas from existing OpenAPI spec
rebar3 openapi gen erlang --spec existing_api.yaml --handler existing_handler.erl

# 2. Tool preserves existing implementation code
# 3. Tool adds JSON schema files
# 4. Optionally add annotations to handler for better documentation
```

### Existing Handlers Without OpenAPI Spec

```bash
# 1. Create minimal _openapi_metadata.json
# 2. Create per-operation JSON schemas manually or from examples
# 3. Add annotations to handler (optional)
# 4. Generate OpenAPI spec
rebar3 openapi gen spec --handler existing_handler.erl --output api.yaml
```

---

## 9. Implementation Checklist

To implement the **Erlang → OpenAPI** direction, create these modules:

- [ ] `handler_annotation_parser.erl` - Parse `%%%` comments for metadata
- [ ] `schema_reader.erl` - Read and parse JSON schema files
- [ ] `openapi_assembler.erl` - Combine all sources into OpenAPI structure
- [ ] `openapi_yaml_writer.erl` - Write OpenAPI map to YAML format
- [ ] Update `spec_generator.erl` - Use new modules instead of stub

---

## 10. See Also

- `examples/handlers/annotated_handler.erl` - Template with full annotations
- `examples/schemas/` - Example enhanced JSON schema files
- `examples/specs/sample.yaml` - Example OpenAPI spec
- `requirement.md` - Full project requirements

