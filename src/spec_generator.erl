-module(spec_generator).
-export([generate/1]).

generate(#{handler_path := HandlerPath, app_name := App, output_path := OutPath, format := yaml}) ->
    case file:read_file(HandlerPath) of
        {ok, Bin} ->
            case handler_parser:parse_routes(Bin) of
                {ok, RouteTriples} ->
                    Implemented = handler_parser:implemented_ops(Bin),
                    Kept = [R || R <- RouteTriples, lists:member(maps:get(operation_id, R), Implemented)],
                    Doc = build_openapi(Kept, App),
                    BinOut = yamerl:encode([Doc]),
                    file:write_file(OutPath, BinOut),
                    ok;
                {error, E} -> {error, E}
            end;
        {error, E} -> {error, {file_read_error, HandlerPath, E}}
    end;

generate(_) -> {error, invalid_args}.

build_openapi(Routes, App) ->
    Paths = paths_object(Routes, App),
    #{
        <<"openapi">> => <<"3.0.3">>,
        <<"info">> => #{<<"title">> => <<"API">>, <<"version">> => <<"1.0.0">>},
        <<"paths">> => Paths,
        <<"components">> => #{<<"schemas">> => #{}}
    }.

paths_object(Routes, App) ->
    lists:foldl(fun(R, Acc) ->
        Path = maps:get(path, R),
        Method = string:lowercase(binary_to_list(maps:get(method, R))),
        OpId = maps:get(operation_id, R),
        PathMap = maps:get(Path, Acc, #{}),
        MethodMap = operation_object(OpId, App),
        maps:put(Path, maps:put(list_to_binary(Method), MethodMap, PathMap), Acc)
    end, #{}, Routes).

operation_object(OpId, App) ->
    {ReqSchema, RespSchemas} = load_schemas(App, OpId),
    ReqBody = case ReqSchema of
        undefined ->
            #{<<"required">> => false,
              <<"description">> => <<"Schema definition not found">>};
        S -> #{<<"content">> => #{<<"application/json">> => #{<<"schema">> => S}}}
    end,
    Responses = build_responses(RespSchemas),
    Base = #{<<"operationId">> => OpId, <<"responses">> => Responses},
    case ReqSchema of undefined -> Base#{<<"requestBody">> => ReqBody}; _ -> Base#{<<"requestBody">> => ReqBody} end.

build_responses(RespSchemas) ->
    case maps:size(RespSchemas) of
        0 -> #{<<"default">> => #{<<"description">> => <<"Schema definition not found">>}};
        _ -> maps:from_list([
                {K, resp_obj(S)} || {K, S} <- maps:to_list(RespSchemas)
            ])
    end.

resp_obj(undefined) -> #{<<"description">> => <<"Schema definition not found">>};
resp_obj(S) -> #{<<"description">> => <<"Response">>, <<"content">> => #{<<"application/json">> => #{<<"schema">> => S}}}.

load_schemas(App, OpId) ->
    Dir = filename:join(["apps", App, "priv", "json_schemas"]),
    File = filename:join(Dir, filename(OpId)),
    case file:read_file(File) of
        {ok, Bin} ->
            case jsx:decode(Bin, [return_maps]) of
                Schemas when is_map(Schemas) ->
                    {maps:get(request, Schemas, undefined), maps:get(responses, Schemas, #{})};
                _ -> {undefined, #{}}
            end;
        _ -> {undefined, #{}}
    end.

filename(OpId) when is_binary(OpId) ->
    filename:basename(binary_to_list(OpId)) ++ ".json".
