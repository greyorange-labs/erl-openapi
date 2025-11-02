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
                        rebar_api:info("✓ Generation completed successfully", []),
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
format_error({yaml_parse_error, Err}) ->
    io_lib:format("Failed to parse OpenAPI YAML: ~p", [Err]);
format_error({file_read_error, Path, Err}) ->
    io_lib:format("Failed to read file ~s: ~p", [Path, Err]);
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
        {error, E} ->
            {error, E}
    end.

binary_to_list_safe(B) when is_binary(B) -> binary_to_list(B);
binary_to_list_safe(L) when is_list(L) -> L.

