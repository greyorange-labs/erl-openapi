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

            %% Get operation IDs
            OperationIds = [maps:get(operation_id, R) || R <- Routes],

            io:format("Generating OpenAPI spec from ~p operations...~n", [length(OperationIds)]),

            %% Read metadata
            {ok, Metadata} = schema_reader:read_metadata(binary_to_list(App)),

            %% Read all operation schemas
            Schemas = schema_reader:read_all(binary_to_list(App), OperationIds),

            io:format("Loaded ~p operation schemas~n", [length(Schemas)]),

            %% Assemble complete OpenAPI spec
            OpenapiSpec = openapi_assembler:assemble(Routes, Schemas, Metadata),

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
