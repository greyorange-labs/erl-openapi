-module(openapi_validator).
-export([validate/1, list_operations/1]).

validate(#{<<"openapi">> := _Version, <<"paths">> := Paths}) when is_map(Paths) ->
    case maps:size(Paths) > 0 of
        true -> ok;
        false -> {error, {validation, empty_paths}}
    end;
validate(_) ->
    {error, {validation, missing_required_fields}}.

list_operations(#{<<"paths">> := Paths}) ->
    lists:flatten([ops_for_path(Path, Methods) || {Path, Methods} <- maps:to_list(Paths)]).

ops_for_path(Path, Methods) when is_map(Methods) ->
    [ op_tuple(Path, Method, Def)
      || {Method, Def} <- maps:to_list(Methods), is_map(Def), is_http_method(Method), has_operation_id(Def) ].

is_http_method(<<"get">>) -> true;
is_http_method(<<"put">>) -> true;
is_http_method(<<"post">>) -> true;
is_http_method(<<"delete">>) -> true;
is_http_method(<<"patch">>) -> true;
is_http_method(_) -> false.

has_operation_id(#{<<"operationId">> := _}) -> true;
has_operation_id(_) -> false.

op_tuple(Path, Method, Def) ->
    OpId = maps:get(<<"operationId">>, Def),
    #{path => Path, method => Method, operation_id => OpId, def => Def}.
