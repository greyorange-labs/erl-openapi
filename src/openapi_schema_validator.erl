-module(openapi_schema_validator).
-export([validate_spec/1, validate_spec_file/1]).

%% Validate OpenAPI 3.x specification structure
validate_spec(Spec) when is_map(Spec) ->
    %% Run all validation checks
    Checks = [
        fun validate_openapi_version/1,
        fun validate_info/1,
        fun validate_paths/1,
        fun validate_path_items/1,
        fun validate_operations/1,
        fun validate_components/1
    ],
    run_validations(Checks, Spec, []).

validate_spec_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, _} ->
            %% File exists, assume it will be loaded by yaml_loader
            ok;
        {error, Reason} ->
            {error, {file_not_found, FilePath, Reason}}
    end.

%% Run a list of validation functions
run_validations([], _Spec, []) ->
    ok;
run_validations([], _Spec, Errors) ->
    {error, lists:reverse(Errors)};
run_validations([Check|Rest], Spec, Errors) ->
    case Check(Spec) of
        ok ->
            run_validations(Rest, Spec, Errors);
        {error, Error} ->
            run_validations(Rest, Spec, [Error|Errors])
    end.

%% Validate OpenAPI version field
validate_openapi_version(#{<<"openapi">> := Version}) when is_binary(Version) ->
    VersionStr = binary_to_list(Version),
    case string:split(VersionStr, ".", all) of
        [Major, Minor, _Patch] ->
            case {list_to_integer(Major), list_to_integer(Minor)} of
                {3, _} -> ok;  % OpenAPI 3.x
                _ ->
                    {error, #{
                        type => invalid_version,
                        field => openapi,
                        value => Version,
                        message => "Only OpenAPI 3.x versions are supported",
                        suggestion => "Change version to 3.0.x or 3.1.x"
                    }}
            end;
        _ ->
            {error, #{
                type => invalid_version_format,
                field => openapi,
                value => Version,
                message => "Version must be in format X.Y.Z",
                suggestion => "Use format like '3.0.3' or '3.1.0'"
            }}
    end;
validate_openapi_version(_) ->
    {error, #{
        type => missing_field,
        field => openapi,
        message => "Required field 'openapi' is missing",
        suggestion => "Add 'openapi: 3.0.3' to the root of your spec"
    }}.

%% Validate info object
validate_info(#{<<"info">> := Info}) when is_map(Info) ->
    case {maps:get(<<"title">>, Info, undefined), maps:get(<<"version">>, Info, undefined)} of
        {undefined, _} ->
            {error, #{
                type => missing_field,
                field => 'info.title',
                message => "Required field 'info.title' is missing",
                suggestion => "Add a title to your info object"
            }};
        {_, undefined} ->
            {error, #{
                type => missing_field,
                field => 'info.version',
                message => "Required field 'info.version' is missing",
                suggestion => "Add a version to your info object"
            }};
        _ ->
            ok
    end;
validate_info(_) ->
    {error, #{
        type => missing_field,
        field => info,
        message => "Required field 'info' is missing",
        suggestion => "Add an info object with title and version"
    }}.

%% Validate paths object exists
validate_paths(#{<<"paths">> := Paths}) when is_map(Paths) ->
    case maps:size(Paths) of
        0 ->
            {error, #{
                type => empty_paths,
                field => paths,
                message => "Paths object is empty",
                suggestion => "Add at least one path to your API"
            }};
        _ ->
            ok
    end;
validate_paths(_) ->
    {error, #{
        type => missing_field,
        field => paths,
        message => "Required field 'paths' is missing",
        suggestion => "Add a paths object with your API endpoints"
    }}.

%% Validate individual path items
validate_path_items(#{<<"paths">> := Paths}) ->
    PathErrors = lists:filtermap(
        fun({Path, _Item}) ->
            case binary:first(Path) of
                $/ ->
                    false;  % Valid path
                _ ->
                    {true, #{
                        type => invalid_path,
                        field => Path,
                        message => io_lib:format("Path '~s' must start with '/'", [Path]),
                        suggestion => "Ensure all paths begin with a forward slash"
                    }}
            end
        end,
        maps:to_list(Paths)
    ),
    case PathErrors of
        [] -> ok;
        [FirstError|_] -> {error, FirstError}
    end;
validate_path_items(_) ->
    ok.

%% Validate operations within path items
validate_operations(#{<<"paths">> := Paths}) ->
    AllOps = lists:flatten([
        extract_operations(Path, Methods)
        || {Path, Methods} <- maps:to_list(Paths),
           is_map(Methods)
    ]),
    
    %% Check for operationId presence and uniqueness
    OpIds = lists:filtermap(
        fun({_Path, _Method, Op}) ->
            case maps:get(<<"operationId">>, Op, undefined) of
                undefined -> false;
                OpId -> {true, OpId}
            end
        end,
        AllOps
    ),
    
    %% Check for duplicate operationIds
    case find_duplicates(OpIds) of
        [] ->
            ok;
        Duplicates ->
            {error, #{
                type => duplicate_operation_ids,
                field => operationId,
                values => Duplicates,
                message => "Found duplicate operationIds",
                suggestion => "Each operationId must be unique across all operations"
            }}
    end;
validate_operations(_) ->
    ok.

%% Extract operations from path item
extract_operations(Path, Methods) ->
    HttpMethods = [<<"get">>, <<"put">>, <<"post">>, <<"delete">>, <<"patch">>, <<"options">>, <<"head">>],
    lists:filtermap(
        fun({Method, Op}) ->
            case lists:member(Method, HttpMethods) andalso is_map(Op) of
                true -> {true, {Path, Method, Op}};
                false -> false
            end
        end,
        maps:to_list(Methods)
    ).

%% Validate components object (if present)
validate_components(#{<<"components">> := Components}) when is_map(Components) ->
    %% Validate $ref references if needed
    %% For now, just check structure
    ok;
validate_components(_) ->
    %% Components is optional
    ok.

%% Find duplicate values in list
find_duplicates(List) ->
    find_duplicates(List, #{}, []).

find_duplicates([], _Seen, Dups) ->
    lists:usort(Dups);
find_duplicates([H|T], Seen, Dups) ->
    case maps:get(H, Seen, undefined) of
        undefined ->
            find_duplicates(T, Seen#{H => true}, Dups);
        _ ->
            find_duplicates(T, Seen, [H|Dups])
    end.

