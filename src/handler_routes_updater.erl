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
                    AddText = case ToAdd of
                        [] -> "";
                        _ ->
                            Entries = string:join([route_entry(Op) || Op <- ToAdd], ",\n"),
                            case ListEmpty of
                                true -> Entries ++ trailer_comment(length(ToAdd));
                                false -> ",\n" ++ Entries ++ trailer_comment(length(ToAdd))
                            end
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
    %% Find routes() function, skipping attributes (-spec, -doc, etc.) and comments
    Lines = string:split(Text, "\n", all),
    case find_routes_function_line(Lines, 0) of
        {ok, StartIdx} ->
            %% Found the "routes() ->" line, now find the list boundaries
            find_list_boundaries(Text, Lines, StartIdx);
        Error ->
            Error
    end.

%% Find the line number where "routes() ->" appears (ignoring attributes and comments)
find_routes_function_line([], _Idx) ->
    {error, routes_block_not_found};
find_routes_function_line([Line | Rest], Idx) ->
    Trimmed = string:trim(Line, leading),
    IsComment = string:prefix(Trimmed, "%") =/= nomatch,
    IsAttribute = string:prefix(Trimmed, "-") =/= nomatch,
    IsRoutesFunc = re:run(Trimmed, "^routes\\s*\\(\\s*\\)\\s*->", [{capture, none}]) =:= match,

    case IsRoutesFunc andalso not IsComment andalso not IsAttribute of
        true -> {ok, Idx};
        false -> find_routes_function_line(Rest, Idx + 1)
    end.

%% Find where to split (before the closing "].")
find_list_boundaries(Text, Lines, StartIdx) ->
    %% Get text from routes() line onward
    RemainingLines = lists:nthtail(StartIdx, Lines),
    RemainingText = string:join(RemainingLines, "\n"),

    %% Find the closing ]. in the remaining text
    case re:run(RemainingText, "\\]\\.", [{capture, first, index}]) of
        {match, [{ClosingPos, _Len}]} ->
            %% Calculate absolute position in original text
            PrefixLines = lists:sublist(Lines, StartIdx),
            PrefixText = string:join(PrefixLines, "\n"),
            PrefixLen = length(PrefixText),

            %% Add newline if there were prefix lines
            AdjustedPrefixLen = case StartIdx > 0 of
                true -> PrefixLen + 1; % +1 for the newline
                false -> PrefixLen
            end,

            AbsoluteClosingPos = AdjustedPrefixLen + ClosingPos,

            %% Split right before ].
            Prefix = lists:sublist(Text, AbsoluteClosingPos),
            Suffix = lists:nthtail(AbsoluteClosingPos, Text),
            {ok, Prefix, Suffix};
        _ ->
            {error, routes_block_not_found}
    end.

route_entry(#{path := Path, method := Method, operation_id := OpId, def := Def}) ->
    NeedsContentTypes = has_request_body(Def),
    %% Generate with expanded format to match handler_generator style
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

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L) -> L.

has_request_body(Def) ->
    case maps:get(<<"requestBody">>, Def, undefined) of
        undefined -> false;
        _ -> true
    end.

trailer_comment(0) -> "\n";
trailer_comment(_N) ->
    "\n        %% AUTO-GENERATED: appended routes at end of routes/0; move near related group if desired\n".
