-module(handler_routes_updater).
-export([update/2]).

update(#{handler_path := HandlerPath, dry_run := DryRun}, Openapi) ->
    case file:read_file(HandlerPath) of
        {ok, Bin} ->
            Text = binary_to_list(Bin),
            case locate_routes_end(Text) of
                {ok, Prefix, Suffix} ->
                    Ops = openapi_validator:list_operations(Openapi),
                    %% Skip those already present
                    ExistingOps = existing_opids(Text),
                    ToAdd = [Op || Op <- Ops, not lists:member(maps:get(operation_id, Op), ExistingOps)],
                    %% Check if list is empty to decide whether to prepend comma
                    ListEmpty = is_empty_list(Prefix),
                    Sep = case ListEmpty of true -> "\n"; false -> ",\n" end,
                    AddText = case ToAdd of
                        [] -> "";
                        _ -> Sep ++ string:join([route_entry(Op) || Op <- ToAdd], ",\n") ++ trailer_comment(length(ToAdd))
                    end,
                    NewText = Prefix ++ AddText ++ Suffix,
                    Diff = diff_preview:unified(Text, NewText, HandlerPath),
                    case DryRun of
                        true -> {ok, HandlerPath, Diff};
                        false -> file:write_file(HandlerPath, list_to_binary(NewText)), {ok, HandlerPath, Diff}
                    end;
                {error, _}=E -> E
            end;
        {error, E} -> {error, {file_read_error, HandlerPath, E}}
    end.

existing_opids(Text) ->
    case re:run(Text, "operation_id\\s*=>\\s*'([^']+)'", [global, {capture, all_but_first, list}]) of
        {match, Matches} -> lists:usort([list_to_binary(Op) || [Op] <- Matches]);
        _ -> []
    end.

is_empty_list(Prefix) ->
    %% Check if the list between [ and current position has any non-whitespace
    case re:run(Prefix, "\\[\\s*$") of
        {match, _} -> true;
        _ -> false
    end.

locate_routes_end(Text) ->
    %% Find routes() -> ... ]. pattern
    case re:run(Text, "routes\\(\\)\\s*->\\s*\\[([\\s\\S]*?)\\]\\.", [{capture, [0, 1], list}]) of
        {match, [FullMatch, InsideList]} ->
            %% Find where the closing ] is
            case string:str(Text, "]." ) of
                0 -> {error, routes_block_not_found};
                Pos ->
                    %% Split before the ]
                    Prefix = lists:sublist(Text, Pos - 1),
                    Suffix = lists:nthtail(Pos - 1, Text),
                    {ok, Prefix, Suffix}
            end;
        _ -> {error, routes_block_not_found}
    end.

route_entry(#{path := Path, method := Method, operation_id := OpId, def := Def}) ->
    NeedsContentTypes = has_request_body(Def),
    MethodKV = io_lib:format("<<\"~s\">> => #{operation_id => '~s'~s}", [Method, OpId, content_types_suffix(NeedsContentTypes)]),
    io_lib:format(
      "        #{~n            path => \"~s\",~n            allowed_methods => #{~s}~n        }",
      [Path, MethodKV]
    ).

content_types_suffix(true) -> ", content_types_accepted => [{<<\"application\">>, <<\"json\">>, '*'}]";
content_types_suffix(false) -> "".

has_request_body(Def) ->
    case maps:get(<<"requestBody">>, Def, undefined) of
        undefined -> false;
        _ -> true
    end.

trailer_comment(0) -> "\n";
trailer_comment(_N) ->
    "\n        %% AUTO-GENERATED: appended routes at end of routes/0; move near related group if desired\n".
