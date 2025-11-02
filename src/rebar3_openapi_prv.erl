-module(rebar3_openapi_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, openapi).
-define(DEPS, [app_discovery]).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 openapi gen erlang --spec path --handler path --app name [--dry-run] [--backup]"},
        {short_desc, "OpenAPI code generation and synchronization"},
        {desc, "Bidirectional sync between OpenAPI specs and Erlang handlers"},
        {opts, [
            {spec, undefined, "spec", string, "Path to OpenAPI YAML file"},
            {handler, undefined, "handler", string, "Path to target handler .erl"},
            {app, undefined, "app", string, "App name for schema placement"},
            {output, undefined, "output", string, "Output path for generated OpenAPI"},
            {dry_run, $d, "dry-run", boolean, "Preview changes without writing"},
            {backup, $b, "backup", boolean, "Create .bak when modifying files"},
            {format, undefined, "format", atom, "Output format: yaml|json (spec gen)"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

format_error(Reason) ->
    case Reason of
        {missing, spec} -> "--spec is required (path to OpenAPI YAML)";
        {missing, handler} -> "--handler is required (path to target handler .erl)";
        {missing, app} -> "--app is required (target app for schema files)";
        {missing, output} -> "--output is required (path to write OpenAPI YAML)";
        {yaml_parse_error, Err} -> io_lib:format("YAML parse error: ~p", [Err]);
        {file_read_error, Path, E} -> io_lib:format("Failed to read ~s: ~p", [Path, E]);
        Other -> io_lib:format("~p", [Other])
    end.

%% Entry point
do(State) ->
    try
        {Args, _} = rebar_state:command_parsed_args(State),
        Command = rebar_state:command_args(State),
        do_command(Command, Args, State)
    catch
        Class:Err:Stack ->
            rebar_api:error("openapi provider failed: ~p:~p~n~p", [Class, Err, Stack]),
            {error, Err}
    end.

%% Handle commands: gen erlang | gen spec
do_command(["gen", "erlang"], Args, State) ->
    Ctx = parse_opts_from_args(Args),
    case validate_codegen_inputs(Ctx) of
        ok -> run_codegen(Ctx, State);
        {error, E} -> {error, E}
    end;

do_command(["gen", "spec"], Args, State) ->
    Ctx = parse_opts_from_args(Args),
    case validate_specgen_inputs(Ctx) of
        ok -> run_specgen(Ctx, State);
        {error, E} -> {error, E}
    end;

do_command(_, _, _) ->
    rebar_api:info("Usage: rebar3 openapi gen <erlang|spec> [options]", []),
    rebar_api:info("  gen erlang: Generate Erlang handlers from OpenAPI spec", []),
    rebar_api:info("  gen spec: Generate OpenAPI spec from Erlang handlers", []),
    {error, {usage, "Invalid command"}}.

parse_opts_from_args(Args) ->
    #{
        spec_path => proplists:get_value(spec, Args),
        handler_path => proplists:get_value(handler, Args),
        app_name => proplists:get_value(app, Args),
        output_path => proplists:get_value(output, Args),
        dry_run => proplists:get_value(dry_run, Args, false),
        backup => proplists:get_value(backup, Args, false),
        format => proplists:get_value(format, Args, yaml)
    }.

validate_codegen_inputs(#{spec_path := Spec, handler_path := Handler, app_name := App}) ->
    case {Spec, Handler, App} of
        {undefined, _, _} -> {error, {missing, spec}};
        {_, undefined, _} -> {error, {missing, handler}};
        {_, _, undefined} -> {error, {missing, app}};
        _ -> ok
    end;
validate_codegen_inputs(_) ->
    {error, {missing, required_args}}.

validate_specgen_inputs(#{handler_path := Handler, app_name := App, output_path := Out}) ->
    case {Handler, App, Out} of
        {undefined, _, _} -> {error, {missing, handler}};
        {_, undefined, _} -> {error, {missing, app}};
        {_, _, undefined} -> {error, {missing, output}};
        _ -> ok
    end;
validate_specgen_inputs(_) ->
    {error, {missing, required_args}}.

run_codegen(Ctx = #{spec_path := SpecPath}, State) ->
    case openapi_yaml_loader:load(SpecPath) of
        {ok, Openapi} ->
            case openapi_validator:validate(Openapi) of
                ok ->
                    rebar_api:info("OpenAPI loaded and validated", []),
                    case codegen_execute(Ctx, Openapi) of
                        ok -> {ok, State};
                        {dry_run, DiffText} ->
                            rebar_api:info("Dry run - no files changed. Proposed diff:~n~s", [DiffText]),
                            {ok, State};
                        {error, E} -> {error, E}
                    end;
                {error, E} -> {error, E}
            end;
        {error, E} -> {error, E}
    end.

run_specgen(Ctx, State) ->
    case spec_generator:generate(Ctx) of
        ok -> {ok, State};
        {error, E} -> {error, E}
    end.

codegen_execute(Ctx = #{app_name := App, handler_path := Handler, dry_run := DryRun, backup := Backup}, Openapi) ->
    Ops = openapi_validator:list_operations(Openapi),
    HandlerPath = binary_to_list(Handler),
    AppName = binary_to_list(App),
    
    case DryRun of
        false -> case Backup of true -> backup_manager:backup_many([HandlerPath]); false -> ok end;
        true -> ok
    end,
    
    %% Check if handler exists
    case filelib:is_file(HandlerPath) of
        false ->
            %% Generate new handler from scratch
            rebar_api:info("Handler file does not exist. Generating new handler...", []),
            ModuleName = filename:basename(HandlerPath, ".erl"),
            case DryRun of
                true ->
                    rebar_api:info("DRY RUN: Would create new handler at ~s", [HandlerPath]),
                    {dry_run, "New handler would be generated"};
                false ->
                    ok = schema_writer:write_all(AppName, Ops),
                    ok = handler_generator:generate_new_handler(HandlerPath, ModuleName, Openapi),
                    rebar_api:info("Generated new handler: ~s", [HandlerPath]),
                    ok
            end;
        true ->
            %% Update existing handler
            rebar_api:info("Handler file exists. Updating...", []),
            ok = schema_writer:write_all(AppName, Ops),
            {ok, _RoutesUpdatedFile, RoutesDiff} = handler_routes_updater:update(Ctx, Openapi),
            {ok, _ClausesUpdatedFile, ClausesDiff} = handler_clauses_updater:update(Ctx, Openapi),
            Diff = diff_preview:merge([RoutesDiff, ClausesDiff]),
            case DryRun of
                true -> {dry_run, Diff};
                false -> 
                    _ = code_formatter:format_file(HandlerPath),
                    ok
            end
    end.
