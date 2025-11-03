# Architecture

Technical architecture and implementation details for rebar3_openapi plugin.

## Table of Contents

- [Overview](#overview)
- [Module Structure](#module-structure)
- [Data Flow](#data-flow)
- [Key Algorithms](#key-algorithms)
- [Extension Points](#extension-points)

---

## Overview

The rebar3_openapi plugin provides bidirectional synchronization between OpenAPI 3.x specifications and Erlang handler modules.

### Core Principles

1. **Idempotency** - Multiple runs with same inputs produce same output
2. **Preservation** - Existing implementations are never overwritten
3. **Validation** - Strict validation before any code generation
4. **Formatting** - All generated code is auto-formatted with erlfmt

###Dependencies

- **jsx** - JSON encoding/decoding
- **yamerl** - YAML parsing for OpenAPI specs
- **erlfmt** - Erlang code formatting (via rebar3 plugin)

---

## Module Structure

### Provider Modules

#### `rebar3_openapi.erl`
Main plugin registration module. Implements the `provider` behavior and registers individual command providers.

```erlang
-module(rebar3_openapi).
-behaviour(provider).
-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_openapi_gen_erlang_prv:init(State),
    {ok, State2} = rebar3_openapi_gen_spec_prv:init(State1),
    {ok, State2}.
```

#### `rebar3_openapi_gen_erlang_prv.erl`
Provider for `openapi_gen_erlang` command. Orchestrates OpenAPI → Erlang generation.

**Responsibilities:**
- Parse command-line arguments
- Validate required arguments
- Load and validate OpenAPI spec
- Delegate to generation modules
- Format error messages

#### `rebar3_openapi_gen_spec_prv.erl`
Provider for `openapi_gen_spec` command. Orchestrates Erlang → OpenAPI generation.

**Responsibilities:**
- Parse command-line arguments
- Validate handler module exists
- Delegate to spec generation modules
- Write output file

### Core Generation Modules

#### `openapi_yaml_loader.erl`
Parses OpenAPI YAML files into Erlang maps.

**Key Functions:**
- `load/1` - Load and parse YAML file
- `proplist_to_map/1` - Convert nested proplists to maps
- Recursively converts all strings to binaries for consistency

#### `openapi_validator.erl`
Validates OpenAPI specifications and enforces naming conventions.

**Key Functions:**
- `validate/1` - Main validation entry point
- `list_operations/1` - Extract all operations from paths
- `validate_operation_ids/1` - Enforce camelCase operationId
- `is_camel_case/1` - Check if string follows camelCase rules

**Validation Rules:**
- operationId must be present for all operations
- operationId must start with lowercase letter (a-z)
- operationId can only contain letters (a-zA-Z) and numbers (0-9)
- No special characters, hyphens, underscores allowed

#### `handler_generator.erl`
Generates new Erlang handler modules from scratch.

**Key Functions:**
- `generate/2` - Generate complete handler module
- `routes_function/1` - Generate `routes/0` function
- `handle_request_clauses/1` - Generate `handle_request/3` clauses
- `route_entry/4` - Format individual route map

**Generated Structure:**
```erlang
-module(handler_name).
-export([routes/0, handle_request/3]).

routes() -> [...].
handle_request(...) -> ...
```

#### `handler_routes_updater.erl`
Updates `routes/0` function in existing handlers.

**Key Functions:**
- `update/2` - Main update entry point
- `insert_new_routes/2` - Add new routes to existing list
- Preserves existing route definitions
- Appends new routes with comment

#### `handler_clauses_updater.erl`
Updates `handle_request/3` function in existing handlers.

**Key Functions:**
- `update/2` - Add new function clauses
- `find_insertion_point/2` - Locate where to insert new clauses
- Preserves existing implementations
- Inserts before catch-all clause

#### `handler_parser.erl`
Parses existing Erlang handler modules.

**Key Functions:**
- `parse/1` - Parse handler file
- `extract_routes/1` - Parse `routes/0` function
- `extract_implemented_ops/1` - Find implemented operations
- Uses regex for pattern matching Erlang code

**Extracted Information:**
- Route paths and methods
- operationIds from routes
- Implemented function clauses
- Code structure for insertion points

#### `schema_writer.erl`
Writes JSON schema files for operations.

**Key Functions:**
- `write_all/3` - Write all schema files + metadata
- `write_one/2` - Write single operation schema
- `write_metadata/2` - Write global metadata file
- `build_operation_json/4` - Construct full operation JSON

**Output Format:**
- Draft-04 JSON Schema
- Complete OpenAPI operation definition
- Pretty-printed JSON with proper indentation

#### `code_formatter.erl`
Formats generated Erlang code using erlfmt.

**Key Functions:**
- `format_file/1` - Format single file
- `find_rebar3/0` - Locate rebar3 executable
- `find_project_root/1` - Find project root directory
- Executes `rebar3 fmt -w <file>` command

**Approach:**
- Uses `os:cmd/1` to execute rebar3 fmt
- Changes to project root before execution
- Handles errors gracefully

### Spec Generation Modules

#### `spec_generator.erl`
Orchestrates Erlang → OpenAPI generation.

**Key Functions:**
- `generate/1` - Main generation entry point
- `combine_routes_with_schemas/2` - Merge route and schema info
- Coordinates between parser, reader, assembler, and writer

#### `schema_reader.erl`
Reads JSON schema files from disk.

**Key Functions:**
- `read_all/2` - Read all operation schemas
- `read_metadata/1` - Read global metadata
- `default_metadata/0` - Provide default OpenAPI structure
- Returns map of operationId → schema content

#### `openapi_assembler.erl`
Assembles final OpenAPI map from components.

**Key Functions:**
- `assemble/3` - Build complete OpenAPI structure
- `build_paths/1` - Construct paths object
- `build_operation/1` - Construct operation object
- Handles schema references and components

**Structure:**
```erlang
#{
  <<"openapi">> => <<"3.0.3">>,
  <<"info">> => Info,
  <<"paths">> => Paths,
  <<"components">> => Components
}
```

#### `openapi_yaml_writer.erl`
Writes OpenAPI map to YAML file.

**Key Functions:**
- `write/2` - Write map to YAML file
- `map_to_yaml/2` - Convert map to YAML with indentation
- `list_to_yaml/2` - Convert list to YAML with indentation
- Custom implementation due to yamerl limitations

**Formatting:**
- Proper indentation (2 spaces per level)
- Empty maps/lists on same line
- Handles nested structures correctly

---

## Data Flow

### OpenAPI → Erlang Flow

```
OpenAPI YAML
    ↓
[openapi_yaml_loader] Parse YAML → Map
    ↓
[openapi_validator] Validate structure + operationIds
    ↓
[cli_runner] Orchestrate generation
    ↓
Check if handler exists
    ↓
    ├─→ New: [handler_generator] Generate from scratch
    │       ↓
    │   [schema_writer] Write all JSON schemas
    │       ↓
    │   [code_formatter] Format with erlfmt
    │
    └─→ Exists: [handler_parser] Parse existing
            ↓
        [handler_routes_updater] Add new routes
            ↓
        [handler_clauses_updater] Add new clauses
            ↓
        [schema_writer] Write new/updated schemas
            ↓
        [code_formatter] Format with erlfmt
```

### Erlang → OpenAPI Flow

```
Handler Module
    ↓
[handler_parser] Extract routes + operations
    ↓
[schema_reader] Read JSON schemas + metadata
    ↓
[spec_generator] Combine route info with schemas
    ↓
[openapi_assembler] Build complete OpenAPI map
    ↓
[openapi_yaml_writer] Write to YAML file
```

---

## Key Algorithms

### Route Deduplication

When updating an existing handler, new routes must not duplicate existing ones:

```erlang
%% In handler_routes_updater.erl
insert_new_routes(ExistingRoutes, NewRoutes) ->
    %% Parse existing routes to extract paths + methods
    ExistingSignatures = extract_signatures(ExistingRoutes),

    %% Filter new routes
    TrulyNewRoutes = lists:filter(
        fun(Route) ->
            Sig = route_signature(Route),
            not lists:member(Sig, ExistingSignatures)
        end,
        NewRoutes
    ),

    %% Append to existing
    ExistingRoutes ++ TrulyNewRoutes.

route_signature(#{path := Path, method := Method}) ->
    {Path, Method}.
```

### Clause Insertion Point

New `handle_request/3` clauses must be inserted before the catch-all clause:

```erlang
%% In handler_clauses_updater.erl
find_insertion_point(FileContent, OperationId) ->
    %% Find the catch-all clause: handle_request(OperationId, _Req, Context)
    CatchAllPattern = "handle_request\\(OperationId,",

    case re:run(FileContent, CatchAllPattern) of
        {match, [{Pos, _}]} ->
            %% Insert new clauses before this position
            Pos;
        nomatch ->
            %% Insert at end of file
            byte_size(FileContent)
    end.
```

### camelCase Validation

Strict validation of operationId format:

```erlang
%% In openapi_validator.erl
is_camel_case(OpId) when is_binary(OpId) ->
    is_camel_case(binary_to_list(OpId));
is_camel_case([First | Rest]) when First >= $a, First =< $z ->
    %% Must start with lowercase letter
    lists:all(fun(C) ->
        (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9)
    end, Rest);
is_camel_case(_) ->
    false.
```

### YAML Custom Writer

Custom YAML writer to handle proper indentation:

```erlang
%% In openapi_yaml_writer.erl
map_to_yaml(Map, Indent) when is_map(Map) ->
    case maps:size(Map) of
        0 ->
            "{}";  % Empty map on same line
        _ ->
            lists:flatten([
                io_lib:format("~n~s~s: ~s", [
                    lists:duplicate(Indent, " "),
                    Key,
                    value_to_yaml(Value, Indent + 2)
                ])
                || {Key, Value} <- maps:to_list(Map)
            ])
    end.
```

---

## Extension Points

### Adding New Validators

To add custom validation rules, extend `openapi_validator.erl`:

```erlang
validate(Openapi) ->
    Validators = [
        fun validate_structure/1,
        fun validate_operation_ids/1,
        fun validate_custom_rule/1  % Add your validator
    ],
    run_validators(Validators, Openapi).
```

### Supporting Additional Schema Versions

To support JSON Schema versions beyond Draft-04:

1. Update `schema_writer.erl` to include schema version in output
2. Modify `openapi_assembler.erl` to handle version-specific features
3. Add conversion functions if needed

### Custom Code Templates

To customize generated code format, modify:

- `handler_generator.erl` - Initial generation templates
- `handler_routes_updater.erl` - Route entry format
- `handler_clauses_updater.erl` - Clause format

### Adding New Commands

To add a new rebar3 command:

1. Create new provider module: `rebar3_openapi_<cmd>_prv.erl`
2. Implement `provider` behavior
3. Register in `rebar3_openapi.erl`

```erlang
-module(rebar3_openapi_mycmd_prv).
-behaviour(provider).
-export([init/1, do/1, format_error/1]).

init(State) ->
    Provider = providers:create([
        {name, mycmd},
        {module, ?MODULE},
        {namespace, openapi},
        {bare, true},
        {deps, [{default, app_discovery}]},
        {example, "rebar3 openapi mycmd"},
        {short_desc, "My custom command"},
        {desc, "Detailed description"},
        {opts, []}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.
```

---

## Testing Strategy

### Unit Tests

Test individual modules in isolation:

```erlang
-module(openapi_validator_tests).
-include_lib("eunit/include/eunit.hrl").

camelcase_validation_test() ->
    ?assert(openapi_validator:is_camel_case(<<"getUserList">>)),
    ?assertNot(openapi_validator:is_camel_case(<<"get-user-list">>)),
    ?assertNot(openapi_validator:is_camel_case(<<"GetUserList">>)).
```

### Integration Tests

Test complete workflows:

```bash
# Test generation from spec
rebar3 openapi_gen_erlang \
  --spec test/fixtures/api.yaml \
  --handler test/output/handler.erl \
  --app test_app

# Verify output
diff test/output/handler.erl test/expected/handler.erl
```

### Property-Based Tests

Use PropEr for generative testing:

```erlang
prop_idempotent_generation() ->
    ?FORALL(Spec, openapi_spec_generator(),
        begin
            {ok, Out1} = generate(Spec),
            {ok, Out2} = generate(Spec),
            Out1 =:= Out2
        end).
```

---

## Performance Considerations

### File I/O Optimization

- Read files once and cache in memory
- Use binary matching for parsing (faster than string operations)
- Batch file writes when possible

### Regex Compilation

Compile regexes once at module load:

```erlang
-define(ROUTE_REGEX, "path => \"([^\"]+)\"").

extract_route(Content) ->
    re:run(Content, ?ROUTE_REGEX, [global, {capture, all_but_first, list}]).
```

### Memory Management

- Use tail recursion for list processing
- Stream large files instead of loading entirely
- Clean up temporary data structures

---

## Future Enhancements

### Planned Features

1. **Batch Processing** - Process multiple handlers in one command
2. **Watch Mode** - Auto-regenerate on file changes
3. **Diff Mode** - Show detailed diffs before applying
4. **Validation Testing** - Generate test cases from schemas
5. **Mock Generation** - Generate mock servers from specs

### Plugin Architecture

Consider plugin system for custom generators:

```erlang
-behaviour(openapi_generator).
-export([generate/2, supported_formats/0]).

generate(Spec, Opts) ->
    %% Custom generation logic
    ok.

supported_formats() ->
    [json, yaml, custom].
```

---

## Contributing

See `CONTRIBUTING.md` for development setup and guidelines.

### Code Style

- Follow OTP conventions
- Use meaningful variable names
- Add type specs for public functions
- Document complex algorithms
- Write unit tests for new modules

### Pull Request Process

1. Create feature branch
2. Implement changes with tests
3. Run full test suite
4. Update documentation
5. Submit PR with clear description
