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

            %% Read all operation schemas (returns map of operation_id => schema)
            SchemaMap = schema_reader:read_all(binary_to_list(App), OperationIds),

            io:format("Loaded ~p operation schemas~n", [maps:size(SchemaMap)]),

            %% Combine route information from handler with schema information from JSON files
            EnrichedOperations = combine_routes_with_schemas(Routes, SchemaMap),

            %% Assemble complete OpenAPI spec
            OpenapiSpec = openapi_assembler:assemble(Routes, EnrichedOperations, Metadata),

            %% Write as YAML
            case openapi_yaml_writer:write(OpenapiSpec, binary_to_list(OutPath)) of
                ok ->
                    io:format("Generated OpenAPI spec: ~s~n", [OutPath]),
                    ok;
                {error, E} ->
                    {error, {yaml_write_error, E}}
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
