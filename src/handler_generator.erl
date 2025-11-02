-module(handler_generator).
-export([generate_new_handler/3]).

%% Generate a complete handler module from scratch
generate_new_handler(HandlerPath, ModuleName, Openapi) ->
    Ops = openapi_validator:list_operations(Openapi),

    %% Build the complete handler module content
    Content = [
        module_header(ModuleName),
        "\n",  %% Blank line after module
        exports(),
        "\n",
        routes_function(Ops),
        "\n",
        handle_request_clauses(Ops),
        catch_all_clause()
    ],

    %% Ensure directory exists
    ok = filelib:ensure_dir(HandlerPath),

    %% Write to file
    ok = file:write_file(HandlerPath, iolist_to_binary(Content)),

    %% Format using erlfmt
    _ = code_formatter:format_file(HandlerPath),
    ok.

module_header(ModuleName) ->
    io_lib:format("-module(~s).\n", [ModuleName]).

exports() ->
    "-export([routes/0, handle_request/3]).\n".

routes_function(Ops) ->
    case Ops of
        [] ->
            "routes() ->\n    [\n    ].\n";
        _ ->
            RouteEntries = [route_entry(Op) || Op <- Ops],
            [
                "routes() ->\n    [\n",
                string:join(RouteEntries, ",\n"),
                "\n    ].\n"
            ]
    end.

route_entry(#{path := Path, method := Method, operation_id := OpId, def := Def}) ->
    NeedsContentTypes = has_request_body(Def),
    %% Generate with expanded format to help erlfmt maintain multi-line structure
    case NeedsContentTypes of
        true ->
            io_lib:format(
                "        #{~n"
                "            path => \"~s\",~n"
                "            allowed_methods => #{~n"
                "                <<\"~s\">> => #{~n"
                "                    operation_id => '~s',~n"
                "                    content_types_accepted => [{<<\"application\">>, <<\"json\">>, '*'}]~n"
                "                }~n"
                "            }~n"
                "        }",
                [Path, Method, to_list(OpId)]
            );
        false ->
            io_lib:format(
                "        #{~n"
                "            path => \"~s\",~n"
                "            allowed_methods => #{~n"
                "                <<\"~s\">> => #{~n"
                "                    operation_id => '~s'~n"
                "                }~n"
                "            }~n"
                "        }",
                [Path, Method, to_list(OpId)]
            )
    end.

has_request_body(Def) ->
    case maps:get(<<"requestBody">>, Def, undefined) of
        undefined -> false;
        _ -> true
    end.

handle_request_clauses(Ops) ->
    [clause_text(Op) || Op <- Ops].

clause_text(#{operation_id := OpId}) ->
    OpStr = to_list(OpId),
    [
        "handle_request('", OpStr, "', #{decoded_req_body := ReqBody} = _Req, _Context) ->\n",
        "    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function\n",
        "    %% {Code, RespBody} = your_controller:handle_", string:lowercase(OpStr), "(ReqBody),\n",
        "    Code = 501,\n",
        "    RespBody = #{message => <<\"Yet to be implemented\">>},\n",
        "    {Code, RespBody};\n"
    ].

catch_all_clause() ->
    "handle_request(OperationId, _Req, Context) ->\n"
    "    RespBody = #{message => <<\"Not implemented\">>},\n"
    "    RespHeaders = #{<<\"content-type\">> => <<\"application/json\">>},\n"
    "    {501, RespBody, Context, RespHeaders}.\n".

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

