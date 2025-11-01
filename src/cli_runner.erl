-module(cli_runner).
-export([gen_erlang/5, gen_spec/3]).

%% gen_erlang(SpecPath, AppName, HandlerPath, DryRunBool, BackupBool)
gen_erlang(SpecPath, App, Handler, DryRun, Backup) ->
    Ctx = #{spec_path => to_bin(SpecPath),
            app_name => to_bin(App),
            handler_path => to_bin(Handler),
            dry_run => DryRun,
            backup => Backup,
            format => yaml},
    case openapi_yaml_loader:load(to_bin(SpecPath)) of
        {ok, Openapi} ->
            case openapi_validator:validate(Openapi) of
                ok ->
                    Ops = openapi_validator:list_operations(Openapi),
                    ok = schema_writer:write_all(binary_to_list(to_bin(App)), Ops),
                    case handler_routes_updater:update(Ctx, Openapi) of
                        {ok, _HF1, Diff1} ->
                            case handler_clauses_updater:update(Ctx, Openapi) of
                                {ok, _HF2, Diff2} ->
                                    io:format("~s~n~s~n", [iolist_to_binary(Diff1), iolist_to_binary(Diff2)]),
                                    ok;
                                {error, E2} -> {error, E2}
                            end;
                        {error, E1} -> {error, E1}
                    end;
                {error, E} -> {error, E}
            end;
        {error, E} -> {error, E}
    end.

%% gen_spec(HandlerPath, AppName, OutputPath)
gen_spec(Handler, App, OutPath) ->
    Ctx = #{handler_path => to_bin(Handler),
            app_name => to_bin(App),
            output_path => to_bin(OutPath),
            format => yaml},
    spec_generator:generate(Ctx).

to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(B) when is_binary(B) -> B.
