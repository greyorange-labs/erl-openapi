-module(schema_writer).
-export([write_all/2, write_all/3]).

%% Write all operation schemas, components, and global metadata
write_all(App, Ops, OpenapiSpec) ->
    BaseDir = filename:join(["apps", App, "priv", "json_schemas"]),
    ok = ensure_dir(BaseDir),

    %% Create subdirectories
    OperationsDir = filename:join(BaseDir, "operations"),
    ComponentsDir = filename:join([BaseDir, "components", "schemas"]),
    ok = ensure_dir(OperationsDir),
    ok = ensure_dir(ComponentsDir),

    %% Write operation files to operations/ directory
    lists:foreach(fun(Op) -> write_operation(OperationsDir, Op) end, Ops),

    %% Extract and write component schemas if they exist
    write_components(ComponentsDir, Ops, OpenapiSpec),

    %% Write global metadata file
    write_metadata(BaseDir, OpenapiSpec),
    ok.

%% Legacy function for backward compatibility
write_all(App, Ops) ->
    lists:foreach(fun(Op) -> write_one_legacy(App, Op) end, Ops),
    ok.

%% Write operation file to operations/ directory
write_operation(Dir, #{path := Path, method := Method, operation_id := OpId, def := Def}) ->
    %% Build complete operation JSON structure
    OperationJson = build_operation_json(Path, Method, OpId, Def),

    File = filename:join(Dir, filename(OpId)),
    Json = jsx:prettify(jsx:encode(OperationJson)),
    file:write_file(File, Json).

%% Legacy writer (minimal schemas only)
write_one_legacy(App, #{operation_id := OpId, def := Def}) ->
    Schemas = schema_draft04_converter:extract_request_response_schemas(Def),
    Dir = filename:join(["apps", App, "priv", "json_schemas"]),
    ok = ensure_dir(Dir),
    File = filename:join(Dir, filename(OpId)),
    Json = jsx:prettify(jsx:encode(Schemas)),
    file:write_file(File, Json).

%% Build complete operation JSON with all metadata
build_operation_json(Path, Method, OpId, Def) ->
    Base = #{
        <<"operation_id">> => to_binary(OpId),
        <<"path">> => Path,
        <<"method">> => list_to_binary(string:uppercase(binary_to_list(Method)))
    },

    %% Add optional fields from OpenAPI definition
    WithSummary = add_if_present(Base, <<"summary">>, Def),
    WithDesc = add_if_present(WithSummary, <<"description">>, Def),
    WithTags = add_if_present(WithDesc, <<"tags">>, Def),
    WithSecurity = add_if_present(WithTags, <<"security">>, Def),
    WithParams = add_if_present(WithSecurity, <<"parameters">>, Def),
    WithReqBody = add_if_present(WithParams, <<"requestBody">>, Def),
    WithResponses = add_if_present(WithReqBody, <<"responses">>, Def),
    WithDeprecated = add_if_present(WithResponses, <<"deprecated">>, Def),

    WithDeprecated.

%% Add field to map if present in source
add_if_present(Map, Key, Source) ->
    case maps:get(Key, Source, undefined) of
        undefined -> Map;
        Value -> Map#{Key => Value}
    end.

%% Extract and write component schemas referenced by operations
write_components(ComponentsDir, Ops, OpenapiSpec) ->
    %% Check if OpenAPI spec has components.schemas
    Components = maps:get(<<"components">>, OpenapiSpec, #{}),
    Schemas = maps:get(<<"schemas">>, Components, #{}),
    
    case maps:size(Schemas) of
        0 -> 
            %% No components defined (inline schemas only)
            ok;
        _ ->
            %% Collect all $ref references from operations
            AllRefs = collect_all_refs(Ops),
            
            %% Recursively collect nested references
            AllRefsWithNested = collect_nested_refs(AllRefs, Schemas, sets:new()),
            
            %% Write each referenced schema
            lists:foreach(
                fun(SchemaName) ->
                    case maps:get(SchemaName, Schemas, undefined) of
                        undefined -> ok;  % Referenced but not defined
                        SchemaBody -> write_component_schema(ComponentsDir, SchemaName, SchemaBody)
                    end
                end,
                sets:to_list(AllRefsWithNested)
            ),
            ok
    end.

%% Collect all $ref references from operations
collect_all_refs(Ops) ->
    lists:flatmap(
        fun(#{def := Def}) ->
            find_all_refs(Def)
        end,
        Ops
    ).

%% Recursively collect nested references (e.g., DebugLogging refs ProcessObject)
collect_nested_refs([], _Schemas, Acc) ->
    Acc;
collect_nested_refs([SchemaName | Rest], Schemas, Acc) ->
    case sets:is_element(SchemaName, Acc) of
        true ->
            %% Already processed
            collect_nested_refs(Rest, Schemas, Acc);
        false ->
            NewAcc = sets:add_element(SchemaName, Acc),
            %% Find refs in this schema's body
            case maps:get(SchemaName, Schemas, undefined) of
                undefined ->
                    collect_nested_refs(Rest, Schemas, NewAcc);
                SchemaBody ->
                    NestedRefs = find_all_refs(SchemaBody),
                    collect_nested_refs(Rest ++ NestedRefs, Schemas, NewAcc)
            end
    end.

%% Recursively find all $ref schema names in a data structure
find_all_refs(Map) when is_map(Map) ->
    case maps:get(<<"$ref">>, Map, undefined) of
        undefined ->
            %% No $ref at this level, recurse into values
            lists:flatmap(fun find_all_refs/1, maps:values(Map));
        RefPath ->
            %% Found a $ref, extract schema name and continue recursing
            case extract_schema_name(RefPath) of
                undefined -> 
                    lists:flatmap(fun find_all_refs/1, maps:values(maps:remove(<<"$ref">>, Map)));
                SchemaName -> 
                    [SchemaName | lists:flatmap(fun find_all_refs/1, maps:values(maps:remove(<<"$ref">>, Map)))]
            end
    end;
find_all_refs(List) when is_list(List) ->
    lists:flatmap(fun find_all_refs/1, List);
find_all_refs(_) ->
    [].

%% Extract schema name from $ref path like "#/components/schemas/DebugLogging"
extract_schema_name(<<"#/components/schemas/", SchemaName/binary>>) ->
    SchemaName;
extract_schema_name(_) ->
    undefined.

%% Write a single component schema file
write_component_schema(Dir, SchemaName, SchemaBody) ->
    File = filename:join(Dir, binary_to_list(SchemaName) ++ ".json"),
    Json = jsx:prettify(jsx:encode(SchemaBody)),
    file:write_file(File, Json).

%% Write global metadata file (excluding component schemas)
write_metadata(Dir, OpenapiSpec) ->
    Metadata = #{
        <<"openapi">> => maps:get(<<"openapi">>, OpenapiSpec, <<"3.0.3">>),
        <<"info">> => maps:get(<<"info">>, OpenapiSpec, #{}),
        <<"servers">> => maps:get(<<"servers">>, OpenapiSpec, []),
        <<"security">> => maps:get(<<"security">>, OpenapiSpec, []),
        <<"tags">> => maps:get(<<"tags">>, OpenapiSpec, [])
        %% Note: components.schemas are stored in separate files, not in metadata
    },

    File = filename:join(Dir, "_openapi_metadata.json"),
    Json = jsx:prettify(jsx:encode(Metadata)),
    file:write_file(File, Json).

filename(OpId) when is_binary(OpId) ->
    filename:basename(binary_to_list(OpId)) ++ ".json";
filename(OpId) when is_list(OpId) ->
    filename:basename(OpId) ++ ".json".

ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            %% filelib:ensure_dir needs a file path, not dir path
            %% So we create the dir recursively using filelib:ensure_dir on a dummy file
            ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
            ok
    end.

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).
