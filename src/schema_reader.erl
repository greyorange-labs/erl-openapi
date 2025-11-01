-module(schema_reader).
-export([read_all/2, read_metadata/1]).

%% Read all operation schema files from json_schemas directory
read_all(App, OperationIds) ->
    Dir = filename:join(["apps", App, "priv", "json_schemas"]),
    lists:filtermap(
        fun(OpId) ->
            case read_one(Dir, OpId) of
                {ok, Schema} -> {true, Schema};
                {error, _} -> false
            end
        end,
        OperationIds
    ).

%% Read a single operation schema file
read_one(Dir, OpId) ->
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
        },
        <<"servers">> => [],
        <<"security">> => [],
        <<"tags">> => [],
        <<"components">> => #{}
    }.

filename(OpId) when is_binary(OpId) ->
    filename:basename(binary_to_list(OpId)) ++ ".json";
filename(OpId) when is_list(OpId) ->
    filename:basename(OpId) ++ ".json";
filename(OpId) when is_atom(OpId) ->
    filename:basename(atom_to_list(OpId)) ++ ".json".

