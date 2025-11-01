-module(sample_handler).
-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/butler_shared/v1/logging/debug/disable",
            allowed_methods => #{<<"post">> => #{operation_id => 'DISABLE-DEBUG-LOGGING', content_types_accepted => [{<<"application">>, <<"json">>, '*'}]}}
        },
        #{
            path => "/api/butler_shared/v1/logging/debug/enable",
            allowed_methods => #{<<"post">> => #{operation_id => 'ENABLE-DEBUG-LOGGING', content_types_accepted => [{<<"application">>, <<"json">>, '*'}]}}
        }
    ,
        #{
            path => "/api/butler_shared/v1/logging/level",
            allowed_methods => #{<<"get">> => #{operation_id => 'GET-LOGGING-LEVEL'}}
        },
        #{
            path => "/api/butler_shared/v1/logging/level",
            allowed_methods => #{<<"put">> => #{operation_id => 'SET-LOGGING-LEVEL', content_types_accepted => [{<<"application">>, <<"json">>, '*'}]}}
        }
        %% AUTO-GENERATED: appended routes at end of routes/0; move near related group if desired
].

handle_request('DISABLE-DEBUG-LOGGING', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_disable-debug-logging(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};

handle_request('ENABLE-DEBUG-LOGGING', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_enable-debug-logging(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};


handle_request('GET-LOGGING-LEVEL', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function 
    %% {Code, RespBody} = bsh_logging_http_controller:disable_debug(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};


handle_request('SET-LOGGING-LEVEL', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function 
    %% {Code, RespBody} = bsh_logging_http_controller:disable_debug(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};

handle_request(OperationId, _Req, Context) ->
    RespBody = #{message => <<"Not implemented">>},
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.
