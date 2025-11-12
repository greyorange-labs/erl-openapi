-module(spec_generator).
-export([generate/1]).

%% Generate OpenAPI spec from Erlang handler + JSON schemas
generate(#{handler_path := HandlerPath, app_name := App, output_path := OutPath, format := yaml}) ->
    case file:read_file(HandlerPath) of
        {ok, Bin} ->
            %% Parse handler to get routes
            ParseResult = handler_parser:parse_routes(Bin),
            Routes = case ParseResult of
                {ok, R} -> R;
                {error, _} -> [];
                L when is_list(L) -> L;
                _ -> []
            end,

            %% Get operation IDs from routes
            OperationIds = [maps:get(operation_id, R) || R <- Routes],

            io:format("Generating OpenAPI spec from ~p operations...~n", [length(OperationIds)]),

            %% Read metadata
            {ok, Metadata} = schema_reader:read_metadata(binary_to_list(App)),

            %% Read all operation schemas and component schemas
            {SchemaMap, Components} = schema_reader:read_all(binary_to_list(App), OperationIds),

            io:format("Loaded ~p operation schemas~n", [maps:size(SchemaMap)]),

            %% Combine route information from handler with schema information from JSON files
            EnrichedOperations = combine_routes_with_schemas(Routes, SchemaMap),

            %% Assemble complete OpenAPI spec with components
            OpenapiSpec = openapi_assembler:assemble(Routes, EnrichedOperations, Components, Metadata),

            %% Stage 1: Validate assembled spec structure
            case openapi_schema_validator:validate_spec(OpenapiSpec) of
                ok ->
                    %% Stage 2: Write as YAML
                    case openapi_yaml_writer:write(OpenapiSpec, binary_to_list(OutPath)) of
                        ok ->
                            %% Stage 3: Verify generated YAML can be parsed back
                            case openapi_yaml_loader:load(binary_to_list(OutPath)) of
                                {ok, ReparsedSpec} ->
                                    %% Stage 4: Validate reparsed spec
                                    case openapi_schema_validator:validate_spec(ReparsedSpec) of
                                        ok ->
                                            io:format("Generated OpenAPI spec: ~ts~n", [OutPath]),
                                            io:format("SUCCESS: Validation passed - generated spec is valid OpenAPI 3.x~n"),
                                            ok;
                                        {error, ValidationErrors} ->
                                            io:format("~nERROR: Generated spec failed validation!~n"),
                                            format_validation_errors(ValidationErrors),
                                            io:format("~nThis is a bug in the code generator. Please report it.~n"),
                                            {error, invalid_generated_spec}
                                    end;
                                {error, {yaml_parse_error, ParseError}} ->
                                    io:format("~nERROR: Generated unparseable YAML!~n"),
                                    format_parse_error(ParseError),
                                    io:format("~nThis is a bug in the YAML writer. Please report it.~n"),
                                    {error, invalid_generated_yaml};
                                {error, ParseError} ->
                                    io:format("~nERROR: Failed to parse generated YAML!~n"),
                                    io:format("  Error: ~p~n", [ParseError]),
                                    {error, yaml_parse_failed}
                            end;
                        {error, E} ->
                            {error, {yaml_write_error, E}}
                    end;
                {error, ValidationErrors} ->
                    io:format("~nERROR: Invalid OpenAPI structure before writing!~n"),
                    format_validation_errors(ValidationErrors),
                    io:format("~nThis is a bug in the spec assembler. Please report it.~n"),
                    {error, invalid_spec_structure}
            end;
        {error, E} ->
            {error, {file_read_error, HandlerPath, E}}
    end;

generate(_) ->
    {error, invalid_args}.

%% Combine route information (from handler) with schema information (from JSON files)
%% Routes: list of #{path => "/api/v1/users", method => "post", operation_id => 'create_user'}
%% SchemaMap: map of operation_id => #{request => ..., responses => ...}
%% Returns: list of enriched operations with all info combined
combine_routes_with_schemas(Routes, SchemaMap) ->
    lists:filtermap(
        fun(Route) ->
            OpId = maps:get(operation_id, Route),
            OpIdBin = ensure_binary(OpId),

            %% Look up schema for this operation_id
            case maps:get(OpIdBin, SchemaMap, undefined) of
                undefined ->
                    %% No schema found for this operation, skip it
                    io:format("Warning: No schema found for operation ~p~n", [OpId]),
                    false;
                Schema ->
                    %% Combine route info + schema info
                    EnrichedOp = Schema#{
                        <<"path">> => ensure_binary(maps:get(path, Route)),
                        <<"method">> => ensure_binary(maps:get(method, Route)),
                        <<"operation_id">> => OpIdBin
                    },
                    {true, EnrichedOp}
            end
        end,
        Routes
    ).

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).

%% Format validation errors
format_validation_errors(Errors) when is_list(Errors) ->
    io:format("  Found ~p validation error(s):~n", [length(Errors)]),
    lists:foreach(fun format_single_validation_error/1, Errors);
format_validation_errors(Error) ->
    format_validation_errors([Error]).

format_single_validation_error(#{type := Type, field := Field, message := Msg} = Error) ->
    io:format("~n    * Field: ~p~n", [Field]),
    io:format("      Type: ~p~n", [Type]),
    io:format("      Message: ~ts~n", [Msg]),
    case maps:get(suggestion, Error, undefined) of
        undefined -> ok;
        Sugg -> io:format("      Suggestion: ~ts~n", [Sugg])
    end;
format_single_validation_error(Error) ->
    io:format("    * ~p~n", [Error]).

%% Format parse error
format_parse_error(#{errors := Errors}) when is_list(Errors) ->
    io:format("  Found ~p parsing error(s):~n", [length(Errors)]),
    lists:foreach(fun format_single_parse_error/1, Errors);
format_parse_error(Error) ->
    io:format("  Error: ~p~n", [Error]).

format_single_parse_error(#{line := Line, column := Col, message := Msg}) ->
    io:format("    Line ~p, Column ~p: ~ts~n", [Line, Col, Msg]);
format_single_parse_error(Error) ->
    io:format("    ~p~n", [Error]).
