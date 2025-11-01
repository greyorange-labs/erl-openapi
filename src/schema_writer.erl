-module(schema_writer).
-export([write_all/2]).

write_all(App, Ops) ->
    lists:foreach(fun(Op) -> write_one(App, Op) end, Ops),
    ok.

write_one(App, #{operation_id := OpId, def := Def}) ->
    Schemas = schema_draft04_converter:extract_request_response_schemas(Def),
    Dir = filename:join(["apps", App, "priv", "json_schemas"]),
    ok = ensure_dir(Dir),
    File = filename:join(Dir, filename(OpId)),
    Json = jsx:encode(Schemas),
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
