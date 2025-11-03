-module(openapi_validator).
-export([validate/1, list_operations/1]).

validate(#{<<"openapi">> := _Version, <<"paths">> := Paths}) when is_map(Paths) ->
    case maps:size(Paths) > 0 of
        true ->
            %% Also validate all operationIds are defined and follow camelCase
            validate_operation_ids(Paths);
        false ->
            {error, {validation, empty_paths}}
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

%% Validate all operationIds are defined and follow camelCase convention
validate_operation_ids(Paths) ->
    AllOps = lists:flatten([
        ops_with_validation(Path, Methods)
        || {Path, Methods} <- maps:to_list(Paths)
    ]),

    %% Check if any operations are invalid
    case [Op || Op <- AllOps, is_invalid_op(Op)] of
        [] ->
            ok;
        [FirstInvalid | _] ->
            %% Return the first error found
            FirstInvalid
    end.

%% Extract operations with validation info
ops_with_validation(Path, Methods) when is_map(Methods) ->
    [validate_single_operation(Path, Method, Def)
     || {Method, Def} <- maps:to_list(Methods),
        is_map(Def),
        is_http_method(Method)].

%% Validate a single operation
validate_single_operation(Path, Method, Def) ->
    case maps:get(<<"operationId">>, Def, undefined) of
        undefined ->
            %% Missing operationId
            {error, {invalid_operation_id, #{
                path => Path,
                method => Method,
                operation_id => undefined,
                reason => missing_operation_id
            }}};
        OpId ->
            %% Check if it's camelCase
            case is_camel_case(OpId) of
                true ->
                    ok;
                false ->
                    {error, {invalid_operation_id, #{
                        path => Path,
                        method => Method,
                        operation_id => OpId,
                        reason => not_camel_case
                    }}}
            end
    end.

%% Check if an operation result is invalid (error tuple)
is_invalid_op({error, _}) -> true;
is_invalid_op(ok) -> false.

%% Validate camelCase: starts with lowercase, only letters and numbers
is_camel_case(OpId) when is_binary(OpId) ->
    is_camel_case(binary_to_list(OpId));
is_camel_case([First | Rest]) when First >= $a, First =< $z ->
    %% Starts with lowercase letter, check rest
    lists:all(fun(C) ->
        (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9)
    end, Rest);
is_camel_case(_) ->
    false.
