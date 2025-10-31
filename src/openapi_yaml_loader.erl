-module(openapi_yaml_loader).
-export([load/1]).

load(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try
                Docs = yamerl_constr:string(Bin, [str_node_as_binary]),
                case Docs of
                    [Doc] -> {ok, Doc};
                    _ -> {error, {yaml, multiple_documents_not_supported}}
                end
            catch
                _:Err -> {error, {yaml_parse_error, Err}}
            end;
        {error, E} -> {error, {file_read_error, E}}
    end.
