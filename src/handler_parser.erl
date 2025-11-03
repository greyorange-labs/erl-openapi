-module(handler_parser).
-export([parse_routes/1, implemented_ops/1]).

parse_routes(TextBin) when is_binary(TextBin) ->
    Text = binary_to_list(TextBin),
    case locate_routes_block(Text) of
        {ok, Block} -> extract_routes(Block);
        Error -> Error
    end.

implemented_ops(TextBin) when is_binary(TextBin) ->
    Text = binary_to_list(TextBin),
    Lines = string:tokens(Text, "\n"),
    Ops = [ OpId || Line <- Lines, OpId <- maybe_op_from_clause(Line) ],
    lists:usort(Ops).

maybe_op_from_clause(Line) ->
    %% Skip commented lines
    Trimmed = string:trim(Line, leading),
    IsComment = string:prefix(Trimmed, "%") =/= nomatch,

    case IsComment of
        true -> [];
        false ->
            %% matches: handle_request('OP-ID',
            case re:run(Trimmed, "handle_request\\('([^']+)'", [{capture, [1], list}]) of
                {match, [Op]} -> [list_to_binary(Op)];
                _ -> []
            end
    end.

locate_routes_block(Text) ->
    %% Find routes() function, skipping attributes (-spec, -doc, etc.) and comments
    Lines = string:split(Text, "\n", all),
    case find_routes_function_line(Lines, 0) of
        {ok, StartIdx} ->
            %% Found the "routes() ->" line, now extract the list content
            extract_list_from_position(Text, Lines, StartIdx);
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

%% Extract list content starting from the routes() -> line
extract_list_from_position(_Text, Lines, StartIdx) ->
    %% Get text from routes() line onward
    RemainingLines = lists:nthtail(StartIdx, Lines),
    RemainingText = string:join(RemainingLines, "\n"),

    %% Find the opening [ and closing ].
    case re:run(RemainingText, "\\[([\\s\\S]*?)\\]\\.", [{capture, [1], list}, dotall, ungreedy]) of
        {match, [Block]} -> {ok, Block};
        _ -> {error, routes_block_not_found}
    end.

extract_routes(Block) ->
    %% naive extraction: collect pairs (path, method, opId)
    PathRe = "path\\s*=>\\s*\"([^\"]+)\"",
    MethRe = "<<\\\"([A-Za-z]+)\\\">>\\s*=>",  % Accept both uppercase and lowercase
    OpRe = "operation_id\\s*=>\\s*'([^']+)'",
    Lines = string:tokens(Block, "\n"),
    extract_loop(Lines, undefined, [], PathRe, MethRe, OpRe).

extract_loop([], _CurrPath, Acc, _PathRe, _MethRe, _OpRe) -> lists:reverse(Acc);
extract_loop([Line|Rest], CurrPath, Acc, PathRe, MethRe, OpRe) ->
    NewPath = case re:run(Line, PathRe, [{capture, [1], list}]) of
        {match, [P]} -> list_to_binary(P);
        _ -> CurrPath
    end,
    Method = case re:run(Line, MethRe, [{capture, [1], list}]) of
        {match, [M]} -> list_to_binary(M);
        _ -> undefined
    end,
    case Method of
        undefined -> extract_loop(Rest, NewPath, Acc, PathRe, MethRe, OpRe);
        _ ->
            %% find op id on same or following lines (up to 3 lines lookahead)
            {Op, Rem2} = find_opid([Line|Rest], OpRe),
            case {NewPath, Op} of
                {undefined, _} -> extract_loop(Rest, NewPath, Acc, PathRe, MethRe, OpRe);
                {_, undefined} -> extract_loop(Rest, NewPath, Acc, PathRe, MethRe, OpRe);
                _ -> extract_loop(Rem2, NewPath, [#{path=>NewPath, method=>Method, operation_id=>Op} | Acc], PathRe, MethRe, OpRe)
            end
    end.

find_opid(Lines, OpRe) ->
    find_opid(Lines, OpRe, 0).

find_opid([L|Rest], OpRe, N) when N < 4 ->
    case re:run(L, OpRe, [{capture, [1], list}]) of
        {match, [Op]} -> {list_to_binary(Op), Rest};
        _ -> find_opid(Rest, OpRe, N+1)
    end;
find_opid(Rest, _OpRe, _N) -> {undefined, Rest}.
