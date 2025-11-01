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
    %% matches: handle_request('OP-ID',
    case re:run(Line, "handle_request\\('([^']+)'", [{capture, [1], list}]) of
        {match, [Op]} -> [list_to_binary(Op)];
        _ -> []
    end.

locate_routes_block(Text) ->
    case re:run(Text, "routes\\(\\) ->([\\s\\S]*?)\n\\]\\.", [{capture, [1], list}, dotall]) of
        {match, [Block]} -> {ok, Block};
        _ -> {error, routes_block_not_found}
    end.

extract_routes(Block) ->
    %% naive extraction: collect pairs (path, method, opId)
    PathRe = "path\\s*=>\\s*\"([^\"]+)\"",
    MethRe = "<<\\\"([A-Z]+)\\\">>\\s*=>",
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
