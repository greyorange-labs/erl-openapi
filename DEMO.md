# Demo Guide

This guide demonstrates the bidirectional sync capabilities of the rebar3_openapi plugin.

## Prerequisites

```bash
# Ensure the plugin is compiled
rebar3 compile
```

## Demo 1: Doc-to-Code (OpenAPI → Erlang)

Generate Erlang handler and JSON schemas from an OpenAPI specification.

### Input
- **OpenAPI Spec**: `docs/open-api/order_api.yaml`

### Command

```bash
rebar3 openapi_gen_erlang \
  --spec docs/open-api/order_api.yaml \
  --handler apps/demo_order_service/src/order_handler_generated.erl \
  --app demo_order_service
```

### Expected Output

**Generated Files:**
- `apps/demo_order_service/src/order_handler_generated.erl` - Handler module with routes and function stubs
- `apps/demo_order_service/priv/json_schemas/operations/*.json` - Operation definitions (5 files)
- `apps/demo_order_service/priv/json_schemas/components/schemas/*.json` - Component schemas (7 files)
- `apps/demo_order_service/priv/json_schemas/_openapi_metadata.json` - Global metadata

**Features Demonstrated:**
- ✅ Route generation from paths
- ✅ Function clause generation for each operationId
- ✅ Component schema extraction (OrderInput, OrderResponse, etc.)
- ✅ Nested schema references (Address, OrderItem)
- ✅ Auto-formatting with erlfmt
- ✅ camelCase operationId validation

---

## Demo 2: Code-to-Doc (Erlang → OpenAPI)

Generate OpenAPI specification from existing Erlang handler and JSON schemas.

### Input
- **Handler**: `apps/demo_order_service/src/order_handler.erl`
- **JSON Schemas**: `apps/demo_order_service/priv/json_schemas/`

### Command

```bash
rebar3 openapi_gen_spec \
  --handler apps/demo_order_service/src/order_handler.erl \
  --app demo_order_service \
  --output docs/open-api/order_api_generated.yaml
```

### Expected Output

**Generated File:**
- `docs/open-api/order_api_generated.yaml` - Complete OpenAPI 3.x specification

**Features Demonstrated:**
- ✅ Route extraction from `routes/0` function
- ✅ Operation metadata from JSON files
- ✅ Component schema assembly
- ✅ Proper YAML key ordering (openapi, info, servers, paths, components)
- ✅ Empty fields omitted
- ✅ Nested schema references preserved

### Verification

Compare the generated spec with the original:

```bash
# Should be semantically identical (only formatting differences)
diff -u docs/open-api/order_api.yaml docs/open-api/order_api_generated.yaml
```

---

## Demo 3: Round-Trip Verification

Demonstrate that doc → code → doc produces consistent results.

### Steps

```bash
# 1. Doc to Code
rebar3 openapi_gen_erlang \
  --spec docs/open-api/order_api.yaml \
  --handler apps/demo_order_service/src/order_handler_generated.erl \
  --app demo_order_service

# 2. Code to Doc
rebar3 openapi_gen_spec \
  --handler apps/demo_order_service/src/order_handler_generated.erl \
  --app demo_order_service \
  --output docs/open-api/order_api_roundtrip.yaml

# 3. Compare
diff -u docs/open-api/order_api.yaml docs/open-api/order_api_roundtrip.yaml
```

**Expected Result:** Only formatting differences (key ordering, quotes, whitespace)

---

## Demo 4: Schema Versioning

Demonstrate automatic schema versioning when content changes.

### Steps

```bash
# 1. Generate initial schemas
rebar3 openapi_gen_erlang \
  --spec docs/open-api/order_api.yaml \
  --handler apps/demo_order_service/src/order_handler_v1.erl \
  --app demo_order_service

# 2. Modify the OpenAPI spec (add a field to OrderInput)
# Edit docs/open-api/order_api.yaml:
#   Add "priority" field to OrderInput schema

# 3. Generate again
rebar3 openapi_gen_erlang \
  --spec docs/open-api/order_api.yaml \
  --handler apps/demo_order_service/src/order_handler_v1.erl \
  --app demo_order_service

# 4. Observe versioning output
```

**Expected Output:**
```
⚠️  Schema conflict detected for 'OrderInput'
   Existing: apps/demo_order_service/priv/json_schemas/components/schemas/OrderInput.json
   New version: apps/demo_order_service/priv/json_schemas/components/schemas/OrderInput_v2.json
   → Review both schemas and update $ref paths if needed.
```

**Features Demonstrated:**
- ✅ Content-based comparison
- ✅ Automatic versioning (v2, v3, etc.)
- ✅ $ref path updates in operations
- ✅ Orphaned file detection
- ✅ User warnings

---

## Demo 5: Dry Run Mode

Preview changes without writing files.

```bash
rebar3 openapi_gen_erlang \
  --spec docs/open-api/order_api.yaml \
  --handler apps/demo_order_service/src/order_handler.erl \
  --app demo_order_service \
  --dry-run
```

**Output:**
```
DRY RUN: Would create handler at apps/demo_order_service/src/order_handler.erl
DRY RUN: Would create 5 JSON schema files
```

---

## Cleanup

To reset the demo environment:

```bash
# Remove generated files
rm -f apps/demo_order_service/src/order_handler_generated.erl
rm -f docs/open-api/order_api_generated.yaml
rm -f docs/open-api/order_api_roundtrip.yaml
```

---

## Notes

- All operationIds must be in **camelCase** format
- The plugin is **idempotent** - safe to run multiple times
- Use `--backup` flag to create `.bak` files before modifications
- Component schemas are automatically extracted from OpenAPI specs
- Inline schemas are preserved in operation files


