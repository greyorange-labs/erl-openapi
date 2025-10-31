-module(min_handler).
-export([routes/0, handle_request/3]).

routes() ->
    [
    ].

handle_request(OperationId, _Req, Context) ->
    RespBody = #{message => <<"Not implemented">>},
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.
