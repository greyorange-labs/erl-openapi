-module(user_handler_test2).

-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/v1/users",
            allowed_methods => #{
                <<"get">> => #{
                    operation_id => 'listUsers'
                }
            }
        },
        #{
            path => "/api/v1/users",
            allowed_methods => #{
                <<"post">> => #{
                    operation_id => 'createUser',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        },
        #{
            path => "/api/v1/users/{userId}",
            allowed_methods => #{
                <<"get">> => #{
                    operation_id => 'getUserById'
                }
            }
        }
    ].

handle_request('listUsers', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_listusers(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request('createUser', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_createuser(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request('getUserById', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_getuserbyid(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request(OperationId, _Req, Context) ->
    RespBody = #{message => <<"Not implemented">>},
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.
