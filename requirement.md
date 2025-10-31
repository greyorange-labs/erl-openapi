# Requirements Document: Bidirectional OpenAPI ↔ Erlang Code Generator

## 1. Overview

### 1.1 Purpose
Build a developer tool (CLI or rebar3 plugin) that provides bidirectional synchronization between OpenAPI 3.x documentation and Erlang HTTP handler modules in a multi-app Erlang project workspace.

### 1.2 Goals
- **Reduce manual boilerplate**: Auto-generate handler code from API specifications
- **Maintain documentation**: Generate up-to-date OpenAPI docs from implemented handlers
- **Enforce consistency**: Ensure handler modules follow project patterns
- **Developer-friendly**: Integrate seamlessly into existing Erlang/rebar3 workflows

---

## 2. Technical Context

### 2.1 Project Structure
- Multi-app Erlang project using rebar3
- Each app contains HTTP handler modules implementing business logic for APIs
- Handler modules follow a standardized pattern (see reference implementation)

### 2.2 Handler Module Pattern
Each HTTP handler module **must** contain:

1. **`routes/0` function**
   - Returns list of route definitions
   - Each route specifies: path, methods, content types, and OperationId
   - Pattern reference: `apps/butler_shared/src/interfaces/in/gm_common_http_handler.erl`

2. **`handle_request/3` function**
   - Multiple clauses, one per OperationId
   - Final catch-all clause returns "not implemented" for undefined OperationIds
   - Signature: `handle_request(OperationId, Req, State)`

### 2.3 Schema Files
- Located in `apps/<app_name>/priv/json_schemas/`
- Naming convention: `<OPERATION-ID>.json`
- Format: JSON Schema Draft-04 (`http://json-schema.org/draft-04/schema#`)
- Examples:
  - `apps/butler_shared/priv/json_schemas/ENABLE-DEBUG-LOGGING.json`
  - `apps/butler_shared/priv/json_schemas/DISABLE-DEBUG-LOGGING.json`

### 2.4 Standards & Technologies
- **OpenAPI**: Version 3.x (3.0.x or 3.1.x)
- **JSON Schema**: Draft-04
- **HTTP Framework**: Cowboy (routing patterns)
- **JSON Libraries**: Jesse (validation), JSX (parsing)

---

## 3. Feature 1: OpenAPI → Erlang Code Generation

### 3.1 Purpose
Parse OpenAPI 3.x documentation and generate/update Erlang handler module code with new routes and function stubs.

### 3.2 Inputs
1. **OpenAPI document path** (file path)
2. **App name** (where to store schema JSON files)
3. **Handler module full path** (target Erlang module)

### 3.3 Validation Requirements

#### 3.3.1 OpenAPI Document Validation
The tool must validate that the OpenAPI document:
- ✅ Is valid OpenAPI 3.x format
- ✅ Contains all required fields per OpenAPI 3.x specification
- ✅ Defines complete route information:
  - Path
  - HTTP methods
  - Request/response schemas
  - Parameters (path, query, header, body)
  - Response codes and descriptions
- ✅ **Every route has an `operationId` defined**
- ✅ Schemas are compatible with JSON Schema Draft-04

**On validation failure**: Display clear error messages and exit without making changes.

#### 3.3.2 Handler Module Validation
- ✅ Module file exists at specified path
- ✅ Module contains `routes/0` function (or can be created)
- ✅ Module contains `handle_request/3` function (or can be created)

### 3.4 Code Generation Behavior

#### 3.4.1 Schema File Creation
For each OperationId in the OpenAPI doc:
- Extract request/response schemas
- Create `apps/<app_name>/priv/json_schemas/<OPERATION-ID>.json`
- Format as JSON Schema Draft-04
- **If schema file already exists**: Overwrite with new schema from OpenAPI doc

#### 3.4.2 Update `routes/0` Function
- Parse existing `routes/0` function
- For each route in OpenAPI doc:
  - **If route is new**: Add route definition following the pattern in `gm_common_http_handler.erl`
  - **If route exists with same path but different methods/OperationId**: Update the existing route definition
- Preserve formatting and style of existing routes
- Maintain alphabetical or logical ordering if present

#### 3.4.3 Update `handle_request/3` Function
- For each OperationId in OpenAPI doc:
  - **If function clause doesn't exist**: Generate new clause with TODO comment
    ```erlang
    handle_request('OPERATION-ID', Req, State) ->
        %% TODO: Call appropriate controller to handle this request
        %% Implement business logic here
        {ok, Req, State}.
    ```
  - **If function clause exists**: Update it with new signature/pattern if changed
- Ensure catch-all clause remains at the end:
  ```erlang
  handle_request(OperationId, Req, State) ->
      {not_implemented, OperationId, Req, State}.
  ```

### 3.5 Idempotency
- Running the tool multiple times with the same inputs produces the same result
- No duplicate routes or function clauses
- Existing implementations are preserved unless explicitly changed in OpenAPI doc

---

## 4. Feature 2: Erlang Code → OpenAPI Documentation

### 4.1 Purpose
Extract route information from Erlang handler modules and generate a consolidated OpenAPI 3.x documentation file.

### 4.2 Inputs
1. **Handler module path** (Erlang module to analyze)
2. **App name** (where to search for schema files)
3. **Output path** (where to write the generated OpenAPI doc)

### 4.3 Validation Requirements

#### 4.3.1 Handler Module Validation
- ✅ Handler module file exists
- ✅ Module contains `routes/0` function
- ✅ Module contains `handle_request/3` function with at least one implemented clause

**On validation failure**: Display clear error messages and exit.

### 4.4 Documentation Generation Behavior

#### 4.4.1 Route Discovery
- Parse `routes/0` function to extract all route definitions
- Extract: path, methods, content types, OperationId

#### 4.4.2 Implementation Check
For each OperationId from `routes/0`:
- Check if `handle_request/3` has a **specific clause** for this OperationId
- **Exclude** the catch-all "not implemented" clause
- **Only document routes with implemented function clauses**

#### 4.4.3 Schema Resolution
For each implemented OperationId:
1. Search for schema file: `apps/<app_name>/priv/json_schemas/<OPERATION-ID>.json`
2. **If schema exists**:
   - Parse JSON Schema (Draft-04)
   - Convert to OpenAPI 3.x schema object
   - Include request/response schemas in documentation
3. **If schema doesn't exist**:
   - Document route with basic information (path, method, OperationId)
   - Add placeholder for request/response bodies
   - Include note: "Schema definition not found"

#### 4.4.4 OpenAPI Document Structure
Generate valid OpenAPI 3.x document with:
- `openapi: 3.x.x` version
- `info` section (title, version, description)
- `paths` section with all documented routes
- `components.schemas` section with reusable schemas
- Proper references using `$ref` where applicable

### 4.5 Output
- Valid OpenAPI 3.x YAML or JSON file
- Well-formatted and human-readable
- Compatible with standard OpenAPI tools (Swagger UI, Redoc, etc.)

---

## 5. Tool Implementation

### 5.1 Tool Type
**Preference**: Rebar3 plugin (for seamless integration) OR standalone CLI tool (for flexibility)

**Recommended approach**: Rebar3 plugin with optional CLI wrapper

### 5.2 Command Interface

#### 5.2.1 OpenAPI → Erlang
```bash
# As rebar3 plugin
rebar3 openapi gen erlang \
  --spec path/to/openapi.yaml \
  --app butler_shared \
  --handler apps/butler_shared/src/interfaces/in/my_handler.erl

# As CLI tool
openapi-erlang-tool gen-code \
  --spec path/to/openapi.yaml \
  --app butler_shared \
  --handler apps/butler_shared/src/interfaces/in/my_handler.erl
```

#### 5.2.2 Erlang → OpenAPI
```bash
# As rebar3 plugin
rebar3 openapi gen spec \
  --handler apps/butler_shared/src/interfaces/in/my_handler.erl \
  --app butler_shared \
  --output docs/api.yaml

# As CLI tool
openapi-erlang-tool gen-spec \
  --handler apps/butler_shared/src/interfaces/in/my_handler.erl \
  --app butler_shared \
  --output docs/api.yaml
```

### 5.3 Error Handling
- Clear, actionable error messages
- Non-zero exit codes on failure
- Validation errors should specify line numbers and specific issues
- Backup original files before modification (optional --no-backup flag)

### 5.4 Dry-Run Mode
```bash
--dry-run  # Show what would be changed without making changes
```

---

## 6. Reference Files

### 6.1 Pattern Reference
- **Handler module pattern**: `apps/butler_shared/src/interfaces/in/gm_common_http_handler.erl`

### 6.2 Schema Examples
- `apps/butler_shared/priv/json_schemas/ENABLE-DEBUG-LOGGING.json`
- `apps/butler_shared/priv/json_schemas/DISABLE-DEBUG-LOGGING.json`

### 6.3 External Documentation
- [OpenAPI 3.x Specification](https://swagger.io/specification/)
- [JSON Schema Draft-04](http://json-schema.org/draft-04/schema#)
- [Cowboy Router](https://ninenines.eu/docs/en/cowboy/2.9/guide/routing/)
- [Jesse JSON Schema Validator](https://github.com/for-GET/jesse)
- [JSX JSON Parser](https://github.com/talentdeficit/jsx)

---

## 7. Success Criteria

### 7.1 Functional Requirements
- ✅ Successfully parses valid OpenAPI 3.x documents
- ✅ Generates valid Erlang code following project patterns
- ✅ Generates valid OpenAPI 3.x documentation
- ✅ Handles missing schema files gracefully
- ✅ Idempotent operations (safe to run multiple times)
- ✅ Preserves existing implementations when updating

### 7.2 Non-Functional Requirements
- ✅ Clear error messages for validation failures
- ✅ Fast execution (< 5 seconds for typical handler module)
- ✅ No external dependencies beyond standard Erlang/OTP and rebar3
- ✅ Works on Linux, macOS, and Windows

### 7.3 Developer Experience
- ✅ Simple command interface
- ✅ Minimal required arguments
- ✅ Helpful --help documentation
- ✅ Dry-run mode for previewing changes
- ✅ Integration with existing rebar3 workflows

---

## 8. Future Enhancements (Out of Scope)

- Batch processing of multiple handler modules
- Interactive mode with prompts
- Git integration (auto-commit generated changes)
- Validation of handler implementations against schemas
- Auto-generation of EUnit tests from OpenAPI examples
- Support for additional JSON Schema versions

---

**Document Version**: 1.0
**Last Updated**: 2025-10-31
**Status**: Ready for Implementation


## 9. Implementation Preferences (from Q&A)

### 9.1 Decided Choices

- **Tool form**: Rebar3 plugin first; optional CLI wrapper later.
- **Target versions**: Erlang/OTP 27.3, rebar3 3.25.1.1.
- **Dependencies**: `jsx` (JSON), `jesse` (JSON Schema Draft-04 validation), `yamerl` (YAML).
- **Handler pattern reference**: `/Users/amar.c/workspace/gm_core/butler_server_develop/apps/butler_shared/src/interfaces/in/gm_common_http_handler.erl`.
- **OpenAPI input format**: YAML only.
- **OpenAPI versions**: 3.0.x and 3.1.x supported (best-effort for 3.1.x).
- **OperationId handling**: No normalization; use exactly as in handler modules (quoted atoms as needed).
- **routes/0 updates**: Preserve existing ordering and comments; append new routes at the end and include a comment prompting users to relocate near related routes if desired.
- **handle_request/3 stub**: Match project style, e.g.:

  ```erlang
  handle_request('NEW-OPS-ID', #{decoded_req_body := ReqBody} = _Req, _Context) ->
      %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
      %% {Code, RespBody} = bsh_logging_http_controller:disable_debug(ReqBody),
      Code = 501,
      RespBody = #{message => <<"Yet to be implemented">>},
      {Code, RespBody};
  ```

- **Cowboy specifics**: No custom macros/records beyond standard patterns.
- **Error messages**: Verbose with suggestions and line references.
- **Backups**: Configurable via command-line flag to enable/disable backups when modifying files.
- **Schemas**: Focus on `application/json` bodies; convert to JSON Schema Draft-04 files per OperationId.
- **Spec generation strictness**: Only include routes with implemented `handle_request/3` clauses (exclude catch-all).
- **Spec output path**: Mandatory input (no default implicit path).
- **Workspace integration**: This repository will host the tool code; integration into the Erlang workspace will be done later by the user.
- **CI integration**: Deferred for now.

### 9.2 Implications

- The plugin must parse YAML OpenAPI docs (3.0/3.1), extract `operationId` and schemas, and output Draft-04 JSON schema files under `apps/<app>/priv/json_schemas/` with exact `OPERATION-ID.json` names.
- Code generation must update `routes/0` and `handle_request/3` with strict idempotency, preserving formatting and comments, appending new entries at the end with advisory comments.
- Documentation generation must only include operations that have specific implemented clauses in `handle_request/3`.
