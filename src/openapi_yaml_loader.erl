-module(openapi_yaml_loader).
-export([load/1, proplist_to_map/1]).

load(Path) ->
    %% Ensure yamerl application is started
    _ = ensure_started(crypto),
    _ = ensure_started(yamerl),
    
    BinPath = case is_binary(Path) of
        true -> binary_to_list(Path);
        false -> Path
    end,
    
    case file:read_file(BinPath) of
        {ok, Bin} ->
            try
                %% yamerl:decode returns a list of documents
                Docs = yamerl:decode(Bin),
                case Docs of
                    [Doc] -> {ok, proplist_to_map(Doc)};
                    _ -> {error, {yaml, multiple_documents_not_supported}}
                end
            catch
                _:Err -> {error, {yaml_parse_error, Err}}
            end;
        {error, E} -> {error, {file_read_error, E}}
    end.

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        Other -> Other
    end.

%% Convert yamerl's nested proplists to maps recursively
proplist_to_map(List) when is_list(List) ->
    case is_proplist(List) of
        true ->
            maps:from_list([{to_binary(K), proplist_to_map(V)} || {K, V} <- List]);
        false ->
            [proplist_to_map(Item) || Item <- List]
    end;
proplist_to_map(Other) ->
    Other.

is_proplist([{_K, _V} | Rest]) -> is_proplist(Rest);
is_proplist([]) -> true;
is_proplist(_) -> false.

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).
