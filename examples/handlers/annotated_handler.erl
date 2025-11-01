-module(annotated_handler).
-export([routes/0, handle_request/3]).

%%% @openapi_info
%%% title: Butler Shared API - Debug Logging Service
%%% version: 1.0.0
%%% description: API for managing debug logging in the Butler system. Debug logging can be dynamically enabled/disabled at runtime for troubleshooting.
%%% contact_name: Platform Team
%%% contact_email: platform-team@example.com

%%% @openapi_servers
%%% - url: https://api.production.example.com | description: Production
%%% - url: https://api.staging.example.com | description: Staging
%%% - url: http://localhost:8080 | description: Local

%%% @openapi_security
%%% - bearerAuth: []

%%% @openapi_tags
%%% - name: Debug Logging | description: Control debug logging behavior

%% @doc Returns list of route definitions for this handler
routes() ->
    [
        %%% @route DISABLE-DEBUG-LOGGING
        %%% summary: Disable debug logging
        %%% description: Disables debug logging and returns the system to normal logging levels.
        %%% permissions: logging:write or admin
        %%% tag: Debug Logging
        #{
            path => "/api/butler_shared/v1/logging/debug/disable",
            allowed_methods => #{
                <<"POST">> => #{
                    operation_id => 'DISABLE-DEBUG-LOGGING',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        },
        %%% @route ENABLE-DEBUG-LOGGING
        %%% summary: Enable debug logging
        %%% description: Enables debug logging with optional auto-disable after TTL expires.
        %%% permissions: logging:write or admin
        %%% note: Debug logging may impact system performance.
        %%% tag: Debug Logging
        #{
            path => "/api/butler_shared/v1/logging/debug/enable",
            allowed_methods => #{
                <<"POST">> => #{
                    operation_id => 'ENABLE-DEBUG-LOGGING',
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        }
    ].

%%% @operation DISABLE-DEBUG-LOGGING
%%% @http_status 200: Debug logging successfully disabled
%%% @http_status 400: Invalid request parameters
%%% @http_status 401: Authentication required
%%% @http_status 403: Insufficient permissions
%%% @http_status 500: Internal server error
handle_request('DISABLE-DEBUG-LOGGING', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% Extract request fields
    Reason = maps:get(<<"reason">>, ReqBody),
    Source = maps:get(<<"source">>, ReqBody, <<"manual">>),
    UserId = maps:get(<<"user_id">>, ReqBody, undefined),

    %% Business logic here
    %% {Code, RespBody} = logging_controller:disable_debug(Reason, Source, UserId),

    %% Example response
    Code = 200,
    RespBody = #{
        status => <<"success">>,
        message => <<"Debug logging disabled successfully">>,
        timestamp => <<"2025-11-01T10:30:00Z">>,
        previous_state => #{
            enabled_at => <<"2025-11-01T09:00:00Z">>,
            enabled_by => <<"user-12345">>,
            duration_seconds => 5400
        }
    },
    {Code, RespBody};

%%% @operation ENABLE-DEBUG-LOGGING
%%% @http_status 200: Debug logging successfully enabled
%%% @http_status 400: Invalid request parameters
%%% @http_status 401: Authentication required
%%% @http_status 403: Insufficient permissions
%%% @http_status 500: Internal server error
handle_request('ENABLE-DEBUG-LOGGING', #{decoded_req_body := ReqBody} = _Req, _Context) ->
    %% Extract request fields
    Ttl = maps:get(<<"ttl">>, ReqBody, undefined),
    Reason = maps:get(<<"reason">>, ReqBody, undefined),
    Level = maps:get(<<"level">>, ReqBody, <<"debug">>),
    UserId = maps:get(<<"user_id">>, ReqBody, undefined),

    %% Business logic here
    %% {Code, RespBody} = logging_controller:enable_debug(Ttl, Reason, Level, UserId),

    %% Example response
    Code = 200,
    RespBody = case Ttl of
        undefined ->
            #{
                status => <<"enabled">>,
                message => <<"Debug logging enabled (manual disable required)">>,
                timestamp => <<"2025-11-01T10:30:00Z">>,
                level => Level
            };
        _ ->
            #{
                status => <<"enabled">>,
                message => <<"Debug logging enabled for 1 hour">>,
                timestamp => <<"2025-11-01T10:30:00Z">>,
                expires_at => <<"2025-11-01T11:30:00Z">>,
                level => Level
            }
    end,
    {Code, RespBody};

%%% @operation catch_all
%%% @http_status 501: Operation not implemented
handle_request(OperationId, _Req, _Context) ->
    {501, #{
        error => <<"not_implemented">>,
        message => iolist_to_binary(io_lib:format("Operation ~p not implemented", [OperationId]))
    }}.

