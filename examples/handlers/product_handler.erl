-module(product_handler).

-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/v1/products",
            allowed_methods => #{
                <<"get">> => #{
                    operation_id => 'listProducts'
                }
            }
        },
        #{
            path => "/api/v1/products",
            allowed_methods => #{
                <<"post">> => #{
                    operation_id => 'createProduct',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        }
    ].

handle_request('listProducts', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_listproducts(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request('createProduct', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function
    %% {Code, RespBody} = your_controller:handle_createproduct(ReqBody),
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request(OperationId, _Req, Context) ->
    RespBody = #{message => <<"Not implemented">>},
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.
