-module(rebar3_openapi_prv).
-behaviour(rebar_provider).

-export([init/1, do/1, format_error/1]).

-include_lib("rebar/include/rebar.hrl").

-record(state, {
    command = undefined,
    spec_path = undefined,
    handler_path = undefined,
    app_name = undefined,
    output_path = undefined,
    dry_run = false,
    backup = false,
    format = yaml
}).

init(Providers) ->
    Provider = providers:create([
        {name, openapi},
        {module, ?MODULE},
        {bare, true},
        {deps, []},
        {example, "rebar3 openapi gen <erlang|spec> --spec path --handler path --app name --output path [--dry-run] [--backup]"},
        {opts, [
            {spec, undefined, "--spec", string, "Path to OpenAPI YAML file"},
            {handler, undefined, "--handler", string, "Path to target handler .erl"},
            {app, undefined, "--app", string, "App name for schema placement"},
            {output, undefined, "--output", string, "Output path for generated OpenAPI"},
            {dry_run, false, "--dry-run", boolean, "Preview changes without writing"},
            {backup, false, "--backup", boolean, "Create .bak when modifying files"},
            {format, yaml, "--format", atom, "Output format: yaml|json (spec gen)"}
        ]}
    ]),
    {ok, [Provider | Providers]}.

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
        Command = rebar_state:command_args(State),
        do_command(Command, State)
    catch
        Class:Err:Stack ->
            ?PRV_ERROR("openapi provider failed: ~p:~p\n~p", [Class, Err, Stack]),
            {error, Err}
    end.

%% Handle commands: gen erlang | gen spec
do_command([gen, erlang | Args], _State) ->
    Ctx = parse_opts(Args),
    case validate_codegen_inputs(Ctx) of
        ok -> run_codegen(Ctx);
        {error, E} -> {error, E}
    end;

do_command([gen, spec | Args], _State) ->
    Ctx = parse_opts(Args),
    case validate_specgen_inputs(Ctx) of
        ok -> run_specgen(Ctx);
        {error, E} -> {error, E}
    end;

do_command(_, _) ->
    {error, {usage, "Usage: rebar3 openapi gen <erlang|spec> ..."}}.

parse_opts(Args) ->
    {Opts, _} = getopt:parse(option_specs(), Args),
    #state{
        command = undefined,
        spec_path = proplists:get_value(spec, Opts),
        handler_path = proplists:get_value(handler, Opts),
        app_name = proplists:get_value(app, Opts),
        output_path = proplists:get_value(output, Opts),
        dry_run = proplists:get_value(dry_run, Opts, false),
        backup = proplists:get_value(backup, Opts, false),
        format = proplists:get_value(format, Opts, yaml)
    }.

option_specs() ->
    [
        {spec, undefined, "--spec", string, "Path to OpenAPI YAML"},
        {handler, undefined, "--handler", string, "Path to handler .erl"},
        {app, undefined, "--app", string, "App name"},
        {output, undefined, "--output", string, "Output path for spec"},
        {dry_run, false, "--dry-run", boolean, "Dry-run"},
        {backup, false, "--backup", boolean, "Create backups"},
        {format, yaml, "--format", atom, "yaml|json"}
    ].

validate_codegen_inputs(#state{spec_path=Spec, handler_path=Handler, app_name=App}) ->
    case {Spec, Handler, App} of
        {undefined, _, _} -> {error, {missing, spec}};
        {_, undefined, _} -> {error, {missing, handler}};
        {_, _, undefined} -> {error, {missing, app}};
        _ -> ok
    end.

validate_specgen_inputs(#state{handler_path=Handler, app_name=App, output_path=Out}) ->
    case {Handler, App, Out} of
        {undefined, _, _} -> {error, {missing, handler}};
        {_, undefined, _} -> {error, {missing, app}};
        {_, _, undefined} -> {error, {missing, output}};
        _ -> ok
    end.

run_codegen(Ctx=#state{spec_path=SpecPath}) ->
    case openapi_yaml_loader:load(SpecPath) of
        {ok, Openapi} ->
            case openapi_validator:validate(Openapi) of
                ok ->
                    rebar_api:info("OpenAPI loaded and validated", []),
                    case codegen_execute(Ctx, Openapi) of
                        ok -> ok;
                        {dry_run, DiffText} ->
                            rebar_api:info("Dry run - no files changed. Proposed diff:\n~s", [DiffText]),
                            ok;
                        {error, E} -> {error, E}
                    end;
                {error, E} -> {error, E}
            end;
        {error, E} -> {error, E}
    end.

run_specgen(Ctx) ->
    case spec_generator:generate(Ctx) of
        ok -> ok;
        {error, E} -> {error, E}
    end.

codegen_execute(Ctx=#state{app_name=App, handler_path=Handler, dry_run=DryRun, backup=Backup}, Openapi) ->
    Ops = openapi_validator:list_operations(Openapi),
    case DryRun of
        false -> case Backup of true -> backup_manager:backup_many([Handler]); false -> ok end;
        true -> ok
    end,
    ok = schema_writer:write_all(App, Ops),
    {ok, _RoutesUpdatedFile, RoutesDiff} = handler_routes_updater:update(Ctx, Openapi),
    {ok, _ClausesUpdatedFile, ClausesDiff} = handler_clauses_updater:update(Ctx, Openapi),
    Diff = diff_preview:merge([RoutesDiff, ClausesDiff]),
    case DryRun of
        true -> {dry_run, Diff};
        false -> ok
    end.
