-module(order_handler).

-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/orders",
            allowed_methods => #{
                <<"get">> => #{
                    operation_id => 'listOrders'
                }
            }
        },
        #{
            path => "/orders",
            allowed_methods => #{
                <<"post">> => #{
                    operation_id => 'createOrder',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        },
        #{
            path => "/orders/{orderId}",
            allowed_methods => #{
                <<"get">> => #{
                    operation_id => 'getOrderById'
                }
            }
        },
        #{
            path => "/orders/{orderId}",
            allowed_methods => #{
                <<"put">> => #{
                    operation_id => 'updateOrderStatus',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        },
        #{
            path => "/orders/{orderId}",
            allowed_methods => #{
                <<"delete">> => #{
                    operation_id => 'cancelOrder'
                }
            }
        }
    ].

handle_request('listOrders', #{decoded_req_body := _ReqBody} = _Req, _Context) ->
    %% TODO: Implement business logic
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request('createOrder', #{decoded_req_body := _ReqBody} = _Req, _Context) ->
    %% TODO: Implement business logic
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request('getOrderById', #{decoded_req_body := _ReqBody} = _Req, _Context) ->
    %% TODO: Implement business logic
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request('updateOrderStatus', #{decoded_req_body := _ReqBody} = _Req, _Context) ->
    %% TODO: Implement business logic
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request('cancelOrder', #{decoded_req_body := _ReqBody} = _Req, _Context) ->
    %% TODO: Implement business logic
    Code = 501,
    RespBody = #{message => <<"Yet to be implemented">>},
    {Code, RespBody};
handle_request(OperationId, _Req, Context) ->
    RespBody = #{message => <<"Not implemented">>},
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.


