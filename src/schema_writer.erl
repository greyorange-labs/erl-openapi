-module(schema_writer).
-export([write_all/2, write_all/3]).

%% Write all operation schemas and global metadata
write_all(App, Ops, OpenapiSpec) ->
    Dir = filename:join(["apps", App, "priv", "json_schemas"]),
    ok = ensure_dir(Dir),

    %% Write global metadata file
    write_metadata(Dir, OpenapiSpec),

    %% Write individual operation files with full definitions
    lists:foreach(fun(Op) -> write_one(Dir, Op) end, Ops),
    ok.

%% Legacy function for backward compatibility
write_all(App, Ops) ->
    lists:foreach(fun(Op) -> write_one_legacy(App, Op) end, Ops),
    ok.

%% Write enhanced schema file with complete operation definition
write_one(Dir, #{path := Path, method := Method, operation_id := OpId, def := Def}) ->
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

%% Write global metadata file
write_metadata(Dir, OpenapiSpec) ->
    Metadata = #{
        <<"openapi">> => maps:get(<<"openapi">>, OpenapiSpec, <<"3.0.3">>),
        <<"info">> => maps:get(<<"info">>, OpenapiSpec, #{}),
        <<"servers">> => maps:get(<<"servers">>, OpenapiSpec, []),
        <<"security">> => maps:get(<<"security">>, OpenapiSpec, []),
        <<"tags">> => maps:get(<<"tags">>, OpenapiSpec, []),
        <<"components">> => maps:get(<<"components">>, OpenapiSpec, #{})
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
