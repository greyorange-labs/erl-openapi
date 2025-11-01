-module(handler_clauses_updater).
-export([update/2]).

update(#{handler_path := HandlerPath, dry_run := DryRun}, Openapi) ->
    case file:read_file(HandlerPath) of
        {ok, Bin} ->
            Text = binary_to_list(Bin),
            ExistingOps = handler_parser:implemented_ops(Bin),
            Ops = openapi_validator:list_operations(Openapi),
            ToAdd = [Op || Op <- Ops, not lists:member(maps:get(operation_id, Op), ExistingOps)],
            ClausesText = string:join([clause_text(Op) || Op <- ToAdd], "\n"),
            {Prefix, CatchAllAndRest} = split_before_catch_all(Text),
            NewText = case ClausesText of
                "" -> Text; % nothing to add
                _ -> Prefix ++ ClausesText ++ CatchAllAndRest
            end,
            Diff = diff_preview:unified(Text, NewText, HandlerPath),
            case DryRun of
                true -> {ok, HandlerPath, Diff};
                false ->
                    (Text =:= NewText) orelse file:write_file(HandlerPath, list_to_binary(NewText)),
                    {ok, HandlerPath, Diff}
            end;
        {error, E} -> {error, {file_read_error, HandlerPath, E}}
    end.

split_before_catch_all(Text) ->
    case re:run(Text, "\nhandle_request\\(OperationId,", [{capture, first, index}]) of
        {match, [{Start, _Len}]} ->
            Prefix = lists:sublist(Text, Start),
            Suffix = lists:nthtail(Start, Text),
            {Prefix, Suffix};
        nomatch -> {Text ++ "\n", ""}
    end.

clause_text(#{operation_id := OpId}) ->
    OpStr = to_list(OpId),
    string:join([
        "handle_request('" ++ OpStr ++ "', #{decoded_req_body := ReqBody} = _Req, _Context) ->\n",
        "    %% TODO: Uncomment following, adding relevant business logic or calling relevant logic/resource handler function\n",
        "    %% {Code, RespBody} = bsh_logging_http_controller:disable_debug(ReqBody),\n",
        "    Code = 501,\n",
        "    RespBody = #{message => <<\"Yet to be implemented\">>},\n",
        "    {Code, RespBody};\n"
    ], "").

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.
