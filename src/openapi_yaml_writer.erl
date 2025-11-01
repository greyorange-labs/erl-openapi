-module(openapi_yaml_writer).
-export([write/2]).

%% Write OpenAPI map as YAML file
%% Note: yamerl doesn't have a good encoder, so we'll use a simple custom YAML writer
%% For production, consider using a proper YAML library or JSON output
write(OpenapiMap, OutputPath) ->
    %% Convert map to YAML string
    YamlContent = map_to_yaml(OpenapiMap, 0),
    
    %% Ensure directory exists
    ok = filelib:ensure_dir(OutputPath),
    
    %% Write to file
    file:write_file(OutputPath, YamlContent).

%% Convert Erlang map to YAML string
map_to_yaml(Map, Indent) when is_map(Map) ->
    case maps:size(Map) of
        0 -> "{}";
        _ ->
            IndentStr = lists:duplicate(Indent, $ ),
            Lines = lists:map(
                fun({K, V}) ->
                    KeyStr = to_string(K),
                    ValueStr = value_to_yaml(V, Indent + 2),
                    case is_multiline(ValueStr) of
                        true ->
                            io_lib:format("~s~s:~n~s", [IndentStr, KeyStr, ValueStr]);
                        false ->
                            io_lib:format("~s~s: ~s", [IndentStr, KeyStr, ValueStr])
                    end
                end,
                lists:sort(maps:to_list(Map))
            ),
            string:join(Lines, "\n")
    end.

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
        fun(Item) ->
            ItemStr = value_to_yaml(Item, Indent + 2),
            case is_multiline(ItemStr) of
                true ->
                    io_lib:format("~s-~n~s", [IndentStr, ItemStr]);
                false ->
                    io_lib:format("~s- ~s", [IndentStr, ItemStr])
            end
        end,
        List
    ),
    string:join(Lines, "\n").

%% Format string with proper quoting
format_string(Str) ->
    %% Check if string needs quoting
    NeedsQuotes = lists:any(
        fun(C) -> C =:= $: orelse C =:= $# orelse C =:= $\n end,
        Str
    ),
    case NeedsQuotes of
        true ->
            %% Use double quotes and escape special characters
            Escaped = re:replace(Str, "\"", "\\\\\"", [global, {return, list}]),
            io_lib:format("\"~s\"", [Escaped]);
        false ->
            Str
    end.

%% Check if string representation is multiline
is_multiline(Str) ->
    string:str(Str, "\n") > 0.

%% Check if list is a string
is_string([]) -> true;
is_string([H|T]) when is_integer(H), H >= 0, H =< 1114111 -> is_string(T);
is_string(_) -> false.

%% Convert key to string
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(L) when is_list(L) -> L;
to_string(A) when is_atom(A) -> atom_to_list(A).

