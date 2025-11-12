-module(rebar3_openapi_gen_erlang_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, openapi_gen_erlang).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 openapi_gen_erlang --spec path/to/spec.yaml --handler path/to/handler.erl --app myapp"},
        {short_desc, "Generate Erlang handlers from OpenAPI spec"},
        {desc, "Generate or update Erlang HTTP handlers from OpenAPI 3.x YAML specifications"},
        {opts, [
            {spec, undefined, "spec", string, "Path to OpenAPI YAML file (required)"},
            {handler, undefined, "handler", string, "Path to target handler .erl file (required)"},
            {app, undefined, "app", string, "App name for schema placement (required)"},
            {dry_run, $d, "dry-run", boolean, "Preview changes without writing files"},
            {backup, $b, "backup", boolean, "Create .bak backup when modifying files"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        {Args, _} = rebar_state:command_parsed_args(State),

        %% Extract and validate required arguments
        SpecPath = proplists:get_value(spec, Args),
        HandlerPath = proplists:get_value(handler, Args),
        AppName = proplists:get_value(app, Args),
        DryRun = proplists:get_value(dry_run, Args, false),
        Backup = proplists:get_value(backup, Args, false),

        case validate_args(SpecPath, HandlerPath, AppName) of
            ok ->
                rebar_api:info("Generating Erlang handler from OpenAPI spec...", []),
                rebar_api:info("  Spec: ~s", [SpecPath]),
                rebar_api:info("  Handler: ~s", [HandlerPath]),
                rebar_api:info("  App: ~s", [AppName]),

                %% Call the generation logic
                case generate_erlang(SpecPath, HandlerPath, AppName, DryRun, Backup) of
                    ok ->
                        rebar_api:info("SUCCESS: Generation completed successfully", []),
                        {ok, State};
                    {error, Reason} ->
                        {error, format_error(Reason)}
                end;
            {error, Reason} ->
                {error, format_error(Reason)}
        end
    catch
        Class:Err:Stack ->
            rebar_api:error("openapi_gen_erlang failed: ~p:~p~n~p", [Class, Err, Stack]),
            {error, "Internal error during generation"}
    end.

-spec format_error(any()) -> iolist().
format_error({missing_arg, spec}) ->
    "Missing required argument: --spec <path-to-openapi-yaml>";
format_error({missing_arg, handler}) ->
    "Missing required argument: --handler <path-to-handler-erl>";
format_error({missing_arg, app}) ->
    "Missing required argument: --app <app-name>";
format_error({yaml_parse_error, #{file := File, errors := Errors}}) when is_list(Errors) ->
    ErrorLines = [format_yaml_error(E) || E <- Errors],
    ErrorsFormatted = lists:flatten(lists:join("\n", ErrorLines)),
    lists:flatten(io_lib:format(
        "Failed to parse OpenAPI YAML: ~ts~n~ts~n"
        "Please fix the YAML syntax errors above.",
        [File, ErrorsFormatted]
    ));
format_error({yaml_parse_error, #{message := _Msg, reason := {yamerl_exception, Exceptions}}}) when is_list(Exceptions) ->
    ErrorDetails = openapi_yaml_loader:format_yamerl_errors(Exceptions, "spec"),
    format_error({yaml_parse_error, ErrorDetails});
format_error({yaml_parse_error, #{message := Msg, reason := Reason}}) ->
    io_lib:format("Failed to parse OpenAPI YAML: ~ts~nReason: ~p", [Msg, Reason]);
format_error({yaml_parse_error, Err}) ->
    io_lib:format("Failed to parse OpenAPI YAML: ~p", [Err]);
format_error({schema_validation_failed, Errors}) when is_list(Errors) ->
    ErrorLines = [format_validation_error(E) || E <- Errors],
    ErrorsFormatted = lists:flatten(lists:join("\n", ErrorLines)),
    lists:flatten(io_lib:format(
        "OpenAPI specification validation failed:~n~ts~n"
        "Refer to: https://swagger.io/specification/",
        [ErrorsFormatted]
    ));
format_error({schema_validation_failed, Error}) ->
    format_error({schema_validation_failed, [Error]});
format_error({file_read_error, Path, Err}) ->
    io_lib:format("Failed to read file ~ts: ~p", [Path, Err]);
format_error({invalid_operation_id, #{path := Path, method := Method, operation_id := undefined, reason := missing_operation_id}}) ->
    io_lib:format(
        "ERROR: Missing operationId for ~ts ~ts~n"
        "       All operations must have an operationId defined in camelCase format~n"
        "       Example: operationId: createUser, getUserById, updateOrder",
        [string:to_upper(binary_to_list(Method)), Path]
    );
format_error({invalid_operation_id, #{path := Path, method := Method, operation_id := OpId, reason := not_camel_case}}) ->
    io_lib:format(
        "ERROR: Invalid operationId '~ts' for ~ts ~ts~n"
        "       operationId must be in camelCase format~n"
        "       - Must start with lowercase letter (a-z)~n"
        "       - Can only contain letters (a-zA-Z) and numbers (0-9)~n"
        "       - No hyphens, underscores, or spaces allowed~n"
        "       Examples: createUser, getUserById, updateOrder, deleteItem",
        [OpId, string:to_upper(binary_to_list(Method)), Path]
    );
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

validate_args(undefined, _, _) ->
    {error, {missing_arg, spec}};
validate_args(_, undefined, _) ->
    {error, {missing_arg, handler}};
validate_args(_, _, undefined) ->
    {error, {missing_arg, app}};
validate_args(_, _, _) ->
    ok.

generate_erlang(SpecPath, HandlerPath, AppName, DryRun, Backup) ->
    %% Load and validate OpenAPI spec
    case openapi_yaml_loader:load(SpecPath) of
        {ok, Openapi} ->
            %% First validate OpenAPI structure
            case openapi_schema_validator:validate_spec(Openapi) of
                ok ->
                    %% Then validate operationIds
                    case openapi_validator:validate(Openapi) of
                        ok ->
                            Ops = openapi_validator:list_operations(Openapi),

                            %% Convert to string if binary
                            HandlerPathStr = binary_to_list_safe(HandlerPath),
                            AppNameStr = binary_to_list_safe(AppName),

                            %% Create backup if requested and not dry-run
                            case DryRun of
                                false when Backup ->
                                    case filelib:is_file(HandlerPathStr) of
                                        true -> backup_manager:backup_many([HandlerPathStr]);
                                        false -> ok
                                    end;
                                _ -> ok
                            end,

                            %% Generate handler and schemas
                            case filelib:is_file(HandlerPathStr) of
                                false ->
                                    %% Generate new handler from scratch
                                    rebar_api:info("Handler does not exist. Generating new handler...", []),
                                    case DryRun of
                                        true ->
                                            rebar_api:info("DRY RUN: Would create handler at ~s", [HandlerPathStr]),
                                            rebar_api:info("DRY RUN: Would create ~p JSON schema files", [length(Ops)]),
                                            ok;
                                        false ->
                                            ModuleName = filename:basename(HandlerPathStr, ".erl"),
                                            ok = schema_writer:write_all(AppNameStr, Ops),
                                            ok = handler_generator:generate_new_handler(HandlerPathStr, ModuleName, Openapi),
                                            _ = code_formatter:format_file(HandlerPathStr),
                                            rebar_api:info("Generated new handler: ~s", [HandlerPathStr]),
                                            ok
                                    end;
                                true ->
                                    %% Update existing handler
                                    rebar_api:info("Handler exists. Updating...", []),
                                    Ctx = #{
                                        spec_path => SpecPath,
                                        handler_path => list_to_binary(HandlerPathStr),
                                        app_name => list_to_binary(AppNameStr),
                                        dry_run => DryRun,
                                        backup => Backup
                                    },
                                    case DryRun of
                                        false -> ok = schema_writer:write_all(AppNameStr, Ops);
                                        true -> ok
                                    end,
                                    {ok, _, RoutesDiff} = handler_routes_updater:update(Ctx, Openapi),
                                    {ok, _, ClausesDiff} = handler_clauses_updater:update(Ctx, Openapi),

                                    case DryRun of
                                        true ->
                                            rebar_api:info("DRY RUN: Proposed changes:~n~s~n~s", [RoutesDiff, ClausesDiff]);
                                        false ->
                                            _ = code_formatter:format_file(HandlerPathStr),
                                            ok
                                    end,
                                    ok
                            end;
                        {error, E} ->
                            {error, E}
                    end;
                {error, ValidationErrors} ->
                    {error, {schema_validation_failed, ValidationErrors}}
            end;
        {error, E} ->
            {error, E}
    end.

binary_to_list_safe(B) when is_binary(B) -> binary_to_list(B);
binary_to_list_safe(L) when is_list(L) -> L.

%% Format a single YAML parsing error
format_yaml_error(#{line := Line, column := Col, message := Msg, suggestion := Sugg}) ->
    lists:flatten(io_lib:format("  Line ~p, Column ~p: ~ts~n  Suggestion: ~ts", [Line, Col, Msg, Sugg]));
format_yaml_error(#{message := Msg}) ->
    lists:flatten(io_lib:format("  ~ts", [Msg]));
format_yaml_error(Error) ->
    lists:flatten(io_lib:format("  ~p", [Error])).

%% Format a single validation error
format_validation_error(#{type := _Type, field := Field, message := Msg} = Error) ->
    Suggestion = maps:get(suggestion, Error, ""),
    lists:flatten(io_lib:format("  * ~ts: ~ts~n    ~ts", [format_field_name(Field), Msg, Suggestion]));
format_validation_error(Error) ->
    lists:flatten(io_lib:format("  * ~p", [Error])).

format_field_name(Field) when is_binary(Field) -> binary_to_list(Field);
format_field_name(Field) when is_atom(Field) -> atom_to_list(Field);
format_field_name(Field) -> io_lib:format("~p", [Field]).

