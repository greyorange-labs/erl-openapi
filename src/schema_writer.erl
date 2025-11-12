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

    %% Two-phase approach: determine versions first, then write
    %% Phase 1: Write components and build schema name -> filename mapping
    {SchemaNameMapping, WrittenSchemaFiles} = write_components_with_versioning(ComponentsDir, Ops, OpenapiSpec),

    %% Phase 2: Update $ref paths in operations and write
    lists:foreach(
        fun(Op) ->
            UpdatedOp = update_refs_in_operation(Op, SchemaNameMapping),
            write_operation(OperationsDir, UpdatedOp)
        end,
        Ops
    ),

    %% Detect orphaned schema files
    detect_orphaned_schemas(ComponentsDir, WrittenSchemaFiles),

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

%% Write components with versioning support
%% Returns: {SchemaNameMapping, WrittenFiles}
%%   SchemaNameMapping = #{<<"SchemaName">> => <<"ActualFileName">>}
%%   WrittenFiles = [FileName]
write_components_with_versioning(ComponentsDir, Ops, OpenapiSpec) ->
    %% Check if OpenAPI spec has components.schemas
    Components = maps:get(<<"components">>, OpenapiSpec, #{}),
    Schemas = maps:get(<<"schemas">>, Components, #{}),

    case maps:size(Schemas) of
        0 ->
            %% No components defined (inline schemas only)
            {#{}, []};
        _ ->
            %% Collect all $ref references from operations
            AllRefs = collect_all_refs(Ops),

            %% Recursively collect nested references
            AllRefsWithNested = collect_nested_refs(AllRefs, Schemas, sets:new()),

            %% Write each referenced schema with versioning
            Results = lists:map(
                fun(SchemaName) ->
                    case maps:get(SchemaName, Schemas, undefined) of
                        undefined ->
                            %% Referenced but not defined
                            {SchemaName, SchemaName, undefined};
                        SchemaBody ->
                            ActualFileName = write_component_schema_versioned(ComponentsDir, SchemaName, SchemaBody),
                            {SchemaName, ActualFileName, ActualFileName}
                    end
                end,
                sets:to_list(AllRefsWithNested)
            ),

            %% Build mapping and file list
            Mapping = maps:from_list([{SN, AFN} || {SN, AFN, _} <- Results, AFN =/= undefined]),
            WrittenFiles = [F || {_, _, F} <- Results, F =/= undefined],
            {Mapping, WrittenFiles}
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

%% Write a single component schema file with versioning support
%% Returns the actual filename (without extension) that was written
write_component_schema_versioned(Dir, SchemaName, SchemaBody) ->
    BaseFileName = binary_to_list(SchemaName),
    
    %% Normalize new schema content
    NewContentNormalized = normalize_json(SchemaBody),
    
    %% Check if content matches any existing version (base or versioned)
    case find_matching_version(Dir, BaseFileName, NewContentNormalized) of
        {found, MatchingFileName} ->
            %% Found identical content in existing file
            io:format("   Schema '~s' unchanged (~s)~n", [SchemaName, MatchingFileName]),
            list_to_binary(MatchingFileName);
        not_found ->
            %% No match found, need to write new version
            BaseFile = filename:join(Dir, BaseFileName ++ ".json"),
            case filelib:is_file(BaseFile) of
                false ->
                    %% Base file doesn't exist, write it
                    Json = jsx:prettify(jsx:encode(SchemaBody)),
                    file:write_file(BaseFile, Json),
                    SchemaName;
                true ->
                    %% Base file exists with different content, create versioned file
                    NextVersion = find_next_version(Dir, BaseFileName),
                    VersionedFileName = BaseFileName ++ "_v" ++ integer_to_list(NextVersion),
                    VersionedFile = filename:join(Dir, VersionedFileName ++ ".json"),
                    
                    Json = jsx:prettify(jsx:encode(SchemaBody)),
                    file:write_file(VersionedFile, Json),
                    
                    %% Output warning
                    io:format("~nWARNING: Schema conflict detected for '~s'~n", [SchemaName]),
                    io:format("   Existing: ~s~n", [BaseFile]),
                    io:format("   New version: ~s~n", [VersionedFile]),
                    io:format("   >> Review both schemas and update $ref paths if needed.~n~n", []),
                    
                    list_to_binary(VersionedFileName)
            end
    end.

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

%% Normalize JSON for comparison (canonical form)
normalize_json(Data) ->
    %% Encode and decode to ensure consistent representation
    jsx:encode(Data).

%% Find if content matches any existing version (base or versioned)
%% Returns: {found, FileName} | not_found
find_matching_version(Dir, BaseFileName, NormalizedContent) ->
    %% Check base file first
    BaseFile = filename:join(Dir, BaseFileName ++ ".json"),
    case check_file_content(BaseFile, NormalizedContent) of
        true -> {found, BaseFileName};
        false ->
            %% Check versioned files (v2, v3, v4, ...)
            check_versioned_files(Dir, BaseFileName, NormalizedContent, 2)
    end.

%% Check versioned files recursively
check_versioned_files(Dir, BaseFileName, NormalizedContent, N) ->
    VersionedFileName = BaseFileName ++ "_v" ++ integer_to_list(N),
    VersionedFile = filename:join(Dir, VersionedFileName ++ ".json"),
    case filelib:is_file(VersionedFile) of
        false ->
            %% No more versioned files to check
            not_found;
        true ->
            case check_file_content(VersionedFile, NormalizedContent) of
                true -> {found, VersionedFileName};
                false -> check_versioned_files(Dir, BaseFileName, NormalizedContent, N + 1)
            end
    end.

%% Check if file content matches normalized content
check_file_content(FilePath, NormalizedContent) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            try
                Content = jsx:decode(Binary, [return_maps]),
                Normalized = normalize_json(Content),
                Normalized =:= NormalizedContent
            catch
                _:_ -> false
            end;
        {error, _} ->
            false
    end.

%% Find next available version number for a schema file
%% Returns: Integer (2, 3, 4, etc.)
find_next_version(Dir, BaseFileName) ->
    find_next_version(Dir, BaseFileName, 2).

find_next_version(Dir, BaseFileName, N) ->
    VersionedFile = filename:join(Dir, BaseFileName ++ "_v" ++ integer_to_list(N) ++ ".json"),
    case filelib:is_file(VersionedFile) of
        true -> find_next_version(Dir, BaseFileName, N + 1);
        false -> N
    end.

%% Update $ref paths in an operation based on schema name mapping
update_refs_in_operation(#{def := Def} = Op, SchemaNameMapping) ->
    UpdatedDef = update_refs_recursive(Def, SchemaNameMapping),
    Op#{def => UpdatedDef}.

%% Recursively update $ref paths in a data structure
update_refs_recursive(Map, Mapping) when is_map(Map) ->
    case maps:get(<<"$ref">>, Map, undefined) of
        undefined ->
            %% No $ref at this level, recurse into values
            maps:map(fun(_, V) -> update_refs_recursive(V, Mapping) end, Map);
        RefPath ->
            %% Found a $ref, update it if needed
            case extract_schema_name(RefPath) of
                undefined ->
                    %% Not a component schema ref, leave unchanged
                    maps:map(fun(_, V) -> update_refs_recursive(V, Mapping) end, Map);
                SchemaName ->
                    %% Look up actual filename
                    ActualFileName = maps:get(SchemaName, Mapping, SchemaName),
                    NewRefPath = <<"#/components/schemas/", ActualFileName/binary>>,
                    UpdatedMap = Map#{<<"$ref">> => NewRefPath},
                    %% Continue recursing other fields
                    maps:map(fun(_, V) -> update_refs_recursive(V, Mapping) end, UpdatedMap)
            end
    end;
update_refs_recursive(List, Mapping) when is_list(List) ->
    [update_refs_recursive(Item, Mapping) || Item <- List];
update_refs_recursive(Other, _Mapping) ->
    Other.

%% Detect and warn about orphaned schema files
detect_orphaned_schemas(ComponentsDir, WrittenFiles) ->
    case file:list_dir(ComponentsDir) of
        {ok, AllFiles} ->
            %% Filter to only .json files (exclude directories)
            JsonFiles = [F || F <- AllFiles, lists:suffix(".json", F)],
            
            %% Convert written files to just filenames with extension
            WrittenFileNames = [binary_to_list(F) ++ ".json" || F <- WrittenFiles],
            
            %% Find orphaned files
            OrphanedFiles = JsonFiles -- WrittenFileNames,
            
            case OrphanedFiles of
                [] ->
                    ok;
                _ ->
                    io:format("~nℹ️  Orphaned schema files (no longer referenced):~n", []),
                    lists:foreach(
                        fun(OrphanedFile) ->
                            FullPath = filename:join(ComponentsDir, OrphanedFile),
                            io:format("   ~s~n", [FullPath])
                        end,
                        OrphanedFiles
                    ),
                    io:format("   >> Consider removing if no longer needed.~n~n", [])
            end;
        {error, _} ->
            %% Directory doesn't exist or can't be read
            ok
    end.
