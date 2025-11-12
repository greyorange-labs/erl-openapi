-module(rebar3_openapi_gen_spec_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, openapi_gen_spec).
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
        {example, "rebar3 openapi_gen_spec --handler path/to/handler.erl --app myapp --output path/to/output.yaml"},
        {short_desc, "Generate OpenAPI spec from Erlang handlers"},
        {desc, "Generate OpenAPI 3.x specification from Erlang HTTP handlers"},
        {opts, [
            {handler, undefined, "handler", string, "Path to handler .erl file (required)"},
            {app, undefined, "app", string, "App name (required)"},
            {output, undefined, "output", string, "Output path for generated OpenAPI YAML (required)"},
            {format, undefined, "format", atom, "Output format: yaml (default) or json"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        {Args, _} = rebar_state:command_parsed_args(State),

        %% Extract and validate required arguments
        HandlerPath = proplists:get_value(handler, Args),
        AppName = proplists:get_value(app, Args),
        OutputPath = proplists:get_value(output, Args),
        Format = proplists:get_value(format, Args, yaml),

        case validate_args(HandlerPath, AppName, OutputPath) of
            ok ->
                rebar_api:info("Generating OpenAPI spec from Erlang handler...", []),
                rebar_api:info("  Handler: ~s", [HandlerPath]),
                rebar_api:info("  App: ~s", [AppName]),
                rebar_api:info("  Output: ~s", [OutputPath]),

                %% Call the generation logic
                case generate_spec(HandlerPath, AppName, OutputPath, Format) of
                    ok ->
                        rebar_api:info("SUCCESS: Spec generation completed successfully", []),
                        {ok, State};
                    {error, Reason} ->
                        {error, format_error(Reason)}
                end;
            {error, Reason} ->
                {error, format_error(Reason)}
        end
    catch
        Class:Err:Stack ->
            rebar_api:error("openapi_gen_spec failed: ~p:~p~n~p", [Class, Err, Stack]),
            {error, "Internal error during spec generation"}
    end.

-spec format_error(any()) -> iolist().
format_error({missing_arg, handler}) ->
    "Missing required argument: --handler <path-to-handler-erl>";
format_error({missing_arg, app}) ->
    "Missing required argument: --app <app-name>";
format_error({missing_arg, output}) ->
    "Missing required argument: --output <path-to-output-yaml>";
format_error({file_read_error, Path, Err}) ->
    io_lib:format("Failed to read file ~s: ~p", [Path, Err]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

validate_args(undefined, _, _) ->
    {error, {missing_arg, handler}};
validate_args(_, undefined, _) ->
    {error, {missing_arg, app}};
validate_args(_, _, undefined) ->
    {error, {missing_arg, output}};
validate_args(_, _, _) ->
    ok.

generate_spec(HandlerPath, AppName, OutputPath, Format) ->
    Ctx = #{
        handler_path => ensure_binary(HandlerPath),
        app_name => ensure_binary(AppName),
        output_path => ensure_binary(OutputPath),
        format => Format
    },
    spec_generator:generate(Ctx).

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L).

