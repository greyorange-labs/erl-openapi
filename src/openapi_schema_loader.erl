-module(openapi_schema_loader).

%% Public API
-export([load_schema/2]).

%%------------------------------------------------------------------------------
%% @doc Load and resolve a JSON schema for a given operation.
%%
%% Reads the operation JSON file, extracts the request body schema,
%% and recursively resolves all $ref references to component schemas.
%% Returns a complete Erlang map compatible with jesse:add_schema/2.
%%
%% @param OperationId The operationId (binary) e.g., <<"createOrder">>
%% @param AppName The application name (atom) e.g., my_app
%% @returns {ok, Schema :: map()} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec load_schema(OperationId :: binary(), AppName :: atom()) ->
    {ok, map()} | {error, term()}.
load_schema(OperationId, AppName) ->
    try
        %% Step 1: Load operation JSON file
        OperationFile = operation_file_path(OperationId, AppName),
        case read_json_file(OperationFile) of
            {ok, OperationData} ->
                %% Step 2: Extract request body schema
                case extract_request_schema(OperationData, OperationId) of
                    {ok, RequestSchema} ->
                        %% Step 3: Resolve all $ref references
                        ComponentsDir = components_dir_path(AppName),
                        resolve_refs(RequestSchema, ComponentsDir, []);
                    {error, _} = Error ->
                        Error
                end;
            {error, enoent} ->
                {error, {operation_not_found, OperationId}};
            {error, Reason} ->
                {error, {read_failed, OperationFile, Reason}}
        end
    catch
        Class:ExcReason:Stacktrace ->
            {error, {exception, Class, ExcReason, Stacktrace}}
    end.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

%% @doc Construct the path to an operation JSON file
operation_file_path(OperationId, AppName) ->
    PrivDir = priv_dir(AppName),
    filename:join([
        PrivDir,
        "json_schemas",
        "operations",
        <<OperationId/binary, ".json">>
    ]).

%% @doc Construct the path to the components/schemas directory
components_dir_path(AppName) ->
    PrivDir = priv_dir(AppName),
    filename:join([
        PrivDir,
        "json_schemas",
        "components",
        "schemas"
    ]).

%% @doc Get the priv directory for an application
%% Tries code:priv_dir/1 first, falls back to manual path construction
priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        {error, bad_name} ->
            %% Application not loaded, construct path manually
            %% Assumes apps/{app_name}/priv structure
            filename:join(["apps", atom_to_list(AppName), "priv"]);
        PrivDir ->
            PrivDir
    end.

%% @doc Read and parse a JSON file
read_json_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            try
                Decoded = jsx:decode(Binary, [return_maps]),
                {ok, Decoded}
            catch
                _:_ ->
                    {error, {invalid_json, FilePath}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Extract the request body schema from operation data
extract_request_schema(OperationData, OperationId) ->
    case maps:get(<<"requestBody">>, OperationData, undefined) of
        undefined ->
            {error, {no_request_body, OperationId}};
        RequestBody ->
            case maps:get(<<"content">>, RequestBody, undefined) of
                undefined ->
                    {error, {no_content_in_request_body, OperationId}};
                Content ->
                    case maps:get(<<"application/json">>, Content, undefined) of
                        undefined ->
                            {error, {no_json_content_type, OperationId}};
                        JsonContent ->
                            case maps:get(<<"schema">>, JsonContent, undefined) of
                                undefined ->
                                    {error, {no_schema_in_request_body, OperationId}};
                                Schema ->
                                    {ok, Schema}
                            end
                    end
            end
    end.

%% @doc Recursively resolve $ref references in a schema
%% Visited tracks schema names to prevent circular references
resolve_refs(Schema, ComponentsDir, Visited) when is_map(Schema) ->
    case maps:get(<<"$ref">>, Schema, undefined) of
        undefined ->
            %% No $ref at this level, recursively process all values
            resolve_refs_in_map(Schema, ComponentsDir, Visited);
        RefPath ->
            %% This is a $ref, resolve it
            case parse_ref_path(RefPath) of
                {ok, SchemaName} ->
                    %% Check for circular reference
                    case lists:member(SchemaName, Visited) of
                        true ->
                            {error, {circular_reference, SchemaName}};
                        false ->
                            %% Load the component schema
                            ComponentFile = filename:join(ComponentsDir, <<SchemaName/binary, ".json">>),
                            case read_json_file(ComponentFile) of
                                {ok, ComponentSchema} ->
                                    %% Recursively resolve refs in the component schema
                                    resolve_refs(ComponentSchema, ComponentsDir, [SchemaName | Visited]);
                                {error, enoent} ->
                                    {error, {component_not_found, SchemaName}};
                                {error, Reason} ->
                                    {error, {read_failed, ComponentFile, Reason}}
                            end
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end;
resolve_refs(Schema, _ComponentsDir, _Visited) ->
    %% Non-map values (primitives, lists handled separately)
    {ok, Schema}.

%% @doc Resolve $refs in all values of a map
resolve_refs_in_map(Schema, ComponentsDir, Visited) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            case Acc of
                {error, _} = Error ->
                    Error;
                {ok, ResolvedMap} ->
                    case resolve_refs_in_value(Value, ComponentsDir, Visited) of
                        {ok, ResolvedValue} ->
                            {ok, ResolvedMap#{Key => ResolvedValue}};
                        {error, _} = Error ->
                            Error
                    end
            end
        end,
        {ok, #{}},
        Schema
    ).

%% @doc Resolve $refs in a single value (handles maps, lists, primitives)
resolve_refs_in_value(Value, ComponentsDir, Visited) when is_map(Value) ->
    resolve_refs(Value, ComponentsDir, Visited);
resolve_refs_in_value(Value, ComponentsDir, Visited) when is_list(Value) ->
    resolve_refs_in_list(Value, ComponentsDir, Visited, []);
resolve_refs_in_value(Value, _ComponentsDir, _Visited) ->
    {ok, Value}.

%% @doc Resolve $refs in a list
resolve_refs_in_list([], _ComponentsDir, _Visited, Acc) ->
    {ok, lists:reverse(Acc)};
resolve_refs_in_list([Item | Rest], ComponentsDir, Visited, Acc) ->
    case resolve_refs_in_value(Item, ComponentsDir, Visited) of
        {ok, ResolvedItem} ->
            resolve_refs_in_list(Rest, ComponentsDir, Visited, [ResolvedItem | Acc]);
        {error, _} = Error ->
            Error
    end.

%% @doc Parse a $ref path like "#/components/schemas/OrderInput"
parse_ref_path(<<"#/components/schemas/", SchemaName/binary>>) ->
    {ok, SchemaName};
parse_ref_path(RefPath) ->
    {error, {invalid_ref_path, RefPath}}.

