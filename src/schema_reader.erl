-module(schema_reader).
-export([read_all/2, read_metadata/1, read_components/1]).

%% Read all operation schema files and component schemas
%% Returns {OperationsMap, ComponentsMap}
read_all(App, OperationIds) ->
    BaseDir = filename:join(["apps", App, "priv", "json_schemas"]),
    OperationsDir = filename:join(BaseDir, "operations"),

    %% Read operation files
    Operations = maps:from_list(
        lists:filtermap(
            fun(OpId) ->
                case read_operation(OperationsDir, OpId) of
                    {ok, Schema} ->
                        {true, {ensure_binary(OpId), Schema}};
                    {error, _} -> false
                end
            end,
            OperationIds
        )
    ),

    %% Read component schemas
    Components = read_components(App),

    {Operations, Components}.

%% Read a single operation schema file from operations/ directory
read_operation(Dir, OpId) ->
    File = filename:join(Dir, filename(OpId)),
    case file:read_file(File) of
        {ok, Bin} ->
            try
                Schema = jsx:decode(Bin, [return_maps]),
                {ok, Schema}
            catch
                _:Err -> {error, {json_decode_error, Err}}
            end;
        {error, E} -> {error, {file_read_error, File, E}}
    end.

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).

%% Read all component schema files from components/schemas/ directory
%% Returns map of SchemaName => SchemaBody
read_components(App) ->
    ComponentsDir = filename:join(["apps", App, "priv", "json_schemas", "components", "schemas"]),

    case filelib:is_dir(ComponentsDir) of
        false ->
            %% No components directory (inline schemas only)
            #{};
        true ->
            %% List all .json files in components/schemas/
            case file:list_dir(ComponentsDir) of
                {ok, Files} ->
                    JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],
                    maps:from_list(
                        lists:filtermap(
                            fun(FileName) ->
                                SchemaName = filename:basename(FileName, ".json"),
                                File = filename:join(ComponentsDir, FileName),
                                case file:read_file(File) of
                                    {ok, Bin} ->
                                        try
                                            SchemaBody = jsx:decode(Bin, [return_maps]),
                                            {true, {list_to_binary(SchemaName), SchemaBody}}
                                        catch
                                            _:_ -> false
                                        end;
                                    {error, _} -> false
                                end
                            end,
                            JsonFiles
                        )
                    );
                {error, _} ->
                    #{}
            end
    end.

%% Read global metadata file
read_metadata(App) ->
    Dir = filename:join(["apps", App, "priv", "json_schemas"]),
    File = filename:join(Dir, "_openapi_metadata.json"),
    case file:read_file(File) of
        {ok, Bin} ->
            try
                Metadata = jsx:decode(Bin, [return_maps]),
                {ok, Metadata}
            catch
                _:Err -> {error, {json_decode_error, Err}}
            end;
        {error, enoent} ->
            %% Metadata file doesn't exist, return default
            {ok, default_metadata()};
        {error, E} ->
            {error, {file_read_error, File, E}}
    end.

%% Default metadata if file doesn't exist
default_metadata() ->
    #{
        <<"openapi">> => <<"3.0.3">>,
        <<"info">> => #{
            <<"title">> => <<"API">>,
            <<"version">> => <<"1.0.0">>
        }
        %% Don't include empty servers, security, tags, or components
        %% They are optional and will be omitted if empty
    }.

filename(OpId) when is_binary(OpId) ->
    filename:basename(binary_to_list(OpId)) ++ ".json";
filename(OpId) when is_list(OpId) ->
    filename:basename(OpId) ++ ".json";
filename(OpId) when is_atom(OpId) ->
    filename:basename(atom_to_list(OpId)) ++ ".json".

