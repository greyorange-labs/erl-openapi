-module(openapi_yaml_writer).
-export([write/2]).

%% Write OpenAPI map as YAML file
%% Note: yamerl doesn't have a good encoder, so we'll use a simple custom YAML writer
%% For production, consider using a proper YAML library or JSON output
write(OpenapiMap, OutputPath) ->
    %% Convert map to YAML string with proper ordering
    YamlContent = map_to_yaml_root(OpenapiMap, 0),

    %% Ensure directory exists
    ok = filelib:ensure_dir(OutputPath),

    %% Write to file
    file:write_file(OutputPath, YamlContent).

%% Convert root-level OpenAPI map with proper key ordering
map_to_yaml_root(Map, Indent) when is_map(Map) ->
    %% Define standard OpenAPI key order
    StandardOrder = [
        <<"openapi">>,
        <<"info">>,
        <<"servers">>,
        <<"security">>,
        <<"tags">>,
        <<"paths">>,
        <<"components">>,
        <<"externalDocs">>
    ],

    %% Sort keys according to standard order, then alphabetically for unknown keys
    AllKeys = maps:keys(Map),
    {OrderedKeys, OtherKeys} = lists:partition(
        fun(K) -> lists:member(K, StandardOrder) end,
        AllKeys
    ),

    SortedOrderedKeys = lists:sort(
        fun(A, B) ->
            IndexA = index_of(A, StandardOrder),
            IndexB = index_of(B, StandardOrder),
            IndexA =< IndexB
        end,
        OrderedKeys
    ),

    FinalKeys = SortedOrderedKeys ++ lists:sort(OtherKeys),

    %% Convert to YAML using ordered keys
    IndentStr = lists:duplicate(Indent, $ ),
    Lines = lists:filtermap(
        fun(K) ->
            case maps:get(K, Map, undefined) of
                undefined ->
                    false;
                [] ->
                    %% Empty list - skip it entirely
                    false;
                V when is_map(V), map_size(V) =:= 0 ->
                    %% Empty map - skip it entirely
                    false;
                V ->
                    KeyStr = to_string(K),
                    Line = case V of
                        SubMap when is_map(SubMap) ->
                            %% Non-empty nested map - put on new lines
                            SubYaml = map_to_yaml(SubMap, Indent + 2),
                            io_lib:format("~s~s:~n~s", [IndentStr, KeyStr, SubYaml]);
                        SubList when is_list(SubList) ->
                            case is_string(SubList) of
                                true ->
                                    %% It's a string value
                                    io_lib:format("~s~s: ~s", [IndentStr, KeyStr, format_string(SubList)]);
                                false ->
                                    %% Non-empty list - put on new lines
                                    SubYaml = list_to_yaml(SubList, Indent + 2),
                                    io_lib:format("~s~s:~n~s", [IndentStr, KeyStr, SubYaml])
                            end;
                        _ ->
                            %% Simple value - same line
                            ValueStr = value_to_yaml(V, Indent + 2),
                            io_lib:format("~s~s: ~s", [IndentStr, KeyStr, ValueStr])
                    end,
                    {true, Line}
            end
        end,
        FinalKeys
    ),
    string:join(Lines, "\n").

%% Helper to find index of element in list
index_of(Element, List) ->
    index_of(Element, List, 1).

index_of(_, [], _) -> 999999;
index_of(Element, [Element|_], Index) -> Index;
index_of(Element, [_|Rest], Index) -> index_of(Element, Rest, Index + 1).

%% Convert Erlang map to YAML string
map_to_yaml(Map, Indent) when is_map(Map) ->
    IndentStr = lists:duplicate(Indent, $ ),
    case maps:size(Map) of
        0 ->
            %% Empty map - don't return standalone {}, will be handled by parent
            "";
        _ ->
            Lines = lists:filtermap(
                fun({K, V}) ->
                    %% Filter out empty values at all nesting levels
                    case should_omit_value(V) of
                        true ->
                            false;
                        false ->
                            KeyStr = to_string(K),
                            Line = case V of
                                SubMap when is_map(SubMap) ->
                                    %% Non-empty nested map - put on new lines
                                    SubYaml = map_to_yaml(SubMap, Indent + 2),
                                    io_lib:format("~s~s:~n~s", [IndentStr, KeyStr, SubYaml]);
                                SubList when is_list(SubList) ->
                                    case is_string(SubList) of
                                        true ->
                                            %% It's a string value
                                            io_lib:format("~s~s: ~s", [IndentStr, KeyStr, format_string(SubList)]);
                                        false ->
                                            %% Non-empty list - put on new lines
                                            SubYaml = list_to_yaml(SubList, Indent + 2),
                                            io_lib:format("~s~s:~n~s", [IndentStr, KeyStr, SubYaml])
                                    end;
                                _ ->
                                    %% Simple value - same line
                                    ValueStr = value_to_yaml(V, Indent + 2),
                                    io_lib:format("~s~s: ~s", [IndentStr, KeyStr, ValueStr])
                            end,
                            {true, Line}
                    end
                end,
                lists:sort(maps:to_list(Map))
            ),
            string:join(Lines, "\n")
    end.

%% Check if value should be omitted from YAML output
should_omit_value([]) -> true;  % Empty list
should_omit_value(V) when is_map(V), map_size(V) =:= 0 -> true;  % Empty map
should_omit_value(<<>>) -> true;  % Empty binary
should_omit_value("") -> true;  % Empty string
should_omit_value(undefined) -> true;  % Undefined
should_omit_value(_) -> false.

%% Convert value to YAML string
value_to_yaml(Map, Indent) when is_map(Map) ->
    map_to_yaml(Map, Indent);
value_to_yaml(List, Indent) when is_list(List) ->
    case is_string(List) of
        true ->
            %% It's a string
            format_string(List);
        false ->
            %% It's a list
            list_to_yaml(List, Indent)
    end;
value_to_yaml(Bin, _Indent) when is_binary(Bin) ->
    format_string(binary_to_list(Bin));
value_to_yaml(true, _) ->
    "true";
value_to_yaml(false, _) ->
    "false";
value_to_yaml(null, _) ->
    "null";
value_to_yaml(Num, _) when is_number(Num) ->
    io_lib:format("~p", [Num]);
value_to_yaml(Atom, _) when is_atom(Atom) ->
    atom_to_list(Atom).

%% Convert list to YAML array
list_to_yaml([], _) ->
    "[]";
list_to_yaml(List, Indent) ->
    IndentStr = lists:duplicate(Indent, $ ),
    Lines = lists:map(
        fun(Item) when is_map(Item) ->
            %% Map item - put on new lines with proper indent
            ItemYaml = map_to_yaml(Item, Indent + 2),
            io_lib:format("~s-~n~s", [IndentStr, ItemYaml]);
           (Item) when is_list(Item) ->
            case is_string(Item) of
                true ->
                    %% String item
                    io_lib:format("~s- ~s", [IndentStr, format_string(Item)]);
                false ->
                    %% Nested list
                    ItemYaml = list_to_yaml(Item, Indent + 2),
                    io_lib:format("~s-~n~s", [IndentStr, ItemYaml])
            end;
           (Item) ->
            %% Simple value
            ItemStr = value_to_yaml(Item, Indent + 2),
            io_lib:format("~s- ~s", [IndentStr, ItemStr])
        end,
        List
    ),
    string:join(Lines, "\n").

%% Format string with proper quoting and escaping
format_string(Str) ->
    %% Check if string needs quoting or is multiline
    HasNewline = lists:member($\n, Str),
    HasSpecialChars = lists:any(
        fun(C) ->
            C =:= $: orelse C =:= $# orelse C =:= ${ orelse C =:= $} orelse
            C =:= $[ orelse C =:= $] orelse C =:= $, orelse C =:= $& orelse
            C =:= $* orelse C =:= $! orelse C =:= $| orelse C =:= $> orelse
            C =:= $' orelse C =:= $" orelse C =:= $% orelse C =:= $@ orelse
            C =:= $`
        end,
        Str
    ),
    StartsWithSpecial = case Str of
        [] -> false;
        [H|_] -> H =:= $- orelse H =:= $? orelse H =:= $: orelse H =:= $  
    end,
    
    %% Check if looks like a number or boolean
    LooksLikeNumber = is_numeric_string(Str),
    LooksLikeBoolean = Str =:= "true" orelse Str =:= "false" orelse Str =:= "null",
    
    NeedsQuotes = HasSpecialChars orelse StartsWithSpecial orelse 
                  LooksLikeNumber orelse LooksLikeBoolean,
    
    if
        HasNewline ->
            %% Multiline string - use literal style
            Lines = string:split(Str, "\n", all),
            FormattedLines = ["  " ++ L || L <- Lines],
            "|\n" ++ string:join(FormattedLines, "\n");
        NeedsQuotes ->
            %% Use double quotes and escape special characters
            Escaped = escape_string(Str),
            io_lib:format("\"~s\"", [Escaped]);
        true ->
            Str
    end.

%% Escape special characters in strings
escape_string(Str) ->
    escape_string(Str, []).

escape_string([], Acc) ->
    lists:reverse(Acc);
escape_string([H|T], Acc) ->
    Escaped = case H of
        $"  -> [$", $\\];
        $\\ -> [$\\, $\\];
        $\n -> [$n, $\\];
        $\r -> [$r, $\\];
        $\t -> [$t, $\\];
        C   -> [C]
    end,
    escape_string(T, lists:reverse(Escaped) ++ Acc).

%% Check if string looks like a number
is_numeric_string(Str) ->
    case string:to_float(Str) of
        {_, []} -> true;
        {_, _} ->
            case string:to_integer(Str) of
                {_, []} -> true;
                _ -> false
            end
    end.

%% Check if list is a string (printable characters only)
%% A valid string must have at least one character and all must be printable
is_string([]) -> false;  % Empty list is not a string, it's an empty array
is_string([H|T]) when is_integer(H) ->
    %% Consider it a string only if it contains printable characters
    %% Printable range: space (32) to ~ (126) for ASCII, or valid Unicode
    IsPrintable = (H >= 32 andalso H =< 126) orelse (H >= 128 andalso H =< 1114111),
    IsPrintable andalso is_string_rest(T);
is_string(_) -> false.

%% Check rest of string (can include tabs, newlines, etc.)
is_string_rest([]) -> true;
is_string_rest([H|T]) when is_integer(H), H >= 0, H =< 1114111 ->
    is_string_rest(T);
is_string_rest(_) -> false.

%% Convert key to string
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(L) when is_list(L) -> L;
to_string(A) when is_atom(A) -> atom_to_list(A).

