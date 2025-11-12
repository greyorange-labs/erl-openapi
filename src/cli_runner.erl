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
            %% Validate OpenAPI structure
            case openapi_schema_validator:validate_spec(Openapi) of
                ok ->
                    %% Validate operationIds
                    case openapi_validator:validate(Openapi) of
                        ok ->
                            HandlerPath = binary_to_list(to_bin(Handler)),
                            Ops = openapi_validator:list_operations(Openapi),
                            %% Write enhanced schema files with full operation definitions + metadata
                            ok = schema_writer:write_all(binary_to_list(to_bin(App)), Ops, Openapi),

                            %% Check if handler exists
                            case filelib:is_file(HandlerPath) of
                                false ->
                                    %% Generate new handler from scratch
                                    io:format("Handler file does not exist. Generating new handler...~n"),
                                    ModuleName = filename:basename(HandlerPath, ".erl"),
                                    case DryRun of
                                        true ->
                                            io:format("DRY RUN: Would create new handler at ~s~n", [HandlerPath]),
                                            io:format("Module: ~s~n", [ModuleName]),
                                            io:format("Operations: ~p~n", [[maps:get(operation_id, Op) || Op <- Ops]]),
                                            ok;
                                        false ->
                                            ok = handler_generator:generate_new_handler(HandlerPath, ModuleName, Openapi),
                                            io:format("Generated new handler: ~s~n", [HandlerPath]),
                                            ok
                                    end;
                                true ->
                                    %% Update existing handler
                                    io:format("Handler file exists. Updating...~n"),
                                    case handler_routes_updater:update(Ctx, Openapi) of
                                        {ok, _HF1, Diff1} ->
                                            case handler_clauses_updater:update(Ctx, Openapi) of
                                                {ok, _HF2, Diff2} ->
                                                    io:format("~s~n~s~n", [iolist_to_binary(Diff1), iolist_to_binary(Diff2)]),
                                                    %% Format the updated file
                                                    _ = code_formatter:format_file(HandlerPath),
                                                    ok;
                                                {error, E2} -> {error, E2}
                                            end;
                                        {error, E1} -> {error, E1}
                                    end
                            end;
                        {error, ValidationError} ->
                            format_validation_error(SpecPath, ValidationError)
                    end;
                {error, ValidationErrors} ->
                    format_schema_validation_errors(SpecPath, ValidationErrors)
            end;
        {error, {yaml_parse_error, Details}} ->
            format_yaml_parse_error(SpecPath, Details);
        {error, Other} ->
            format_generic_error(SpecPath, Other)
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

%% Format YAML parsing error with context
format_yaml_parse_error(FilePath, Details) ->
    io:format("~n❌ Failed to parse OpenAPI YAML: ~s~n", [FilePath]),
    case Details of
        #{file := _, errors := Errors} when is_list(Errors) ->
            lists:foreach(fun format_single_yaml_error/1, Errors);
        #{line := Line, column := Col, message := Msg, suggestion := Sugg} ->
            io:format("  Location: Line ~p, Column ~p~n", [Line, Col]),
            io:format("  Problem: ~s~n", [Msg]),
            io:format("  Suggestion: ~s~n", [Sugg]);
        #{reason := Reason, message := Msg} ->
            io:format("  Problem: ~s~n", [Msg]),
            io:format("  Details: ~p~n", [Reason]);
        _ ->
            io:format("  Error: ~p~n", [Details])
    end,
    io:format("~n"),
    {error, yaml_parse_failed}.

%% Format a single YAML error
format_single_yaml_error(#{line := Line, column := Col, message := Msg, suggestion := Sugg}) ->
    io:format("~n  Location: Line ~p, Column ~p~n", [Line, Col]),
    io:format("  Problem: ~s~n", [Msg]),
    io:format("  Suggestion: ~s~n", [Sugg]);
format_single_yaml_error(Error) ->
    io:format("  Error: ~p~n", [Error]).

%% Format OpenAPI schema validation errors
format_schema_validation_errors(FilePath, Errors) when is_list(Errors) ->
    io:format("~n❌ OpenAPI specification validation failed: ~s~n", [FilePath]),
    io:format("~nFound ~p validation error(s):~n", [length(Errors)]),
    lists:foreach(fun format_single_validation_error/1, Errors),
    io:format("~nPlease fix these errors and try again.~n"),
    io:format("Refer to OpenAPI 3.x specification: https://swagger.io/specification/~n~n"),
    {error, schema_validation_failed};
format_schema_validation_errors(FilePath, Error) ->
    format_schema_validation_errors(FilePath, [Error]).

%% Format a single validation error
format_single_validation_error(#{type := Type, field := Field, message := Msg} = Error) ->
    io:format("~n  • ~s~n", [format_error_type(Type)]),
    io:format("    Field: ~s~n", [format_field(Field)]),
    io:format("    Problem: ~s~n", [Msg]),
    case maps:get(suggestion, Error, undefined) of
        undefined -> ok;
        Sugg -> io:format("    Suggestion: ~s~n", [Sugg])
    end,
    case maps:get(value, Error, undefined) of
        undefined -> ok;
        Val -> io:format("    Current value: ~p~n", [Val])
    end;
format_single_validation_error(Error) ->
    io:format("  • ~p~n", [Error]).

%% Format error type
format_error_type(missing_field) -> "Missing required field";
format_error_type(invalid_version) -> "Invalid OpenAPI version";
format_error_type(invalid_version_format) -> "Invalid version format";
format_error_type(empty_paths) -> "Empty paths object";
format_error_type(invalid_path) -> "Invalid path format";
format_error_type(duplicate_operation_ids) -> "Duplicate operationId values";
format_error_type(Type) -> io_lib:format("~p", [Type]).

%% Format field name
format_field(Field) when is_binary(Field) -> binary_to_list(Field);
format_field(Field) when is_atom(Field) -> atom_to_list(Field);
format_field(Field) -> io_lib:format("~p", [Field]).

%% Format operationId validation error
format_validation_error(FilePath, {invalid_operation_id, Details}) ->
    io:format("~n❌ operationId validation failed: ~s~n", [FilePath]),
    io:format("~n  Path: ~s~n", [maps:get(path, Details, "unknown")]),
    io:format("  Method: ~s~n", [maps:get(method, Details, "unknown")]),
    case maps:get(reason, Details, undefined) of
        missing_operation_id ->
            io:format("  Problem: Missing operationId~n"),
            io:format("  Suggestion: Add an operationId in camelCase format (e.g., getUserById)~n");
        not_camel_case ->
            io:format("  Problem: operationId '~s' is not in camelCase format~n", 
                     [maps:get(operation_id, Details, "")]),
            io:format("  Suggestion: Use camelCase format - start with lowercase letter, only alphanumeric~n"),
            io:format("  Example: getUserById, createOrder, updateStatus~n");
        _ ->
            io:format("  Problem: ~p~n", [maps:get(reason, Details, unknown)])
    end,
    io:format("~n"),
    {error, operation_id_validation_failed};
format_validation_error(FilePath, Error) ->
    io:format("~n❌ Validation error in: ~s~n", [FilePath]),
    io:format("  Error: ~p~n~n", [Error]),
    {error, validation_failed}.

%% Format generic error
format_generic_error(FilePath, {file_read_error, Reason}) ->
    io:format("~n❌ Failed to read file: ~s~n", [FilePath]),
    io:format("  Reason: ~p~n", [Reason]),
    io:format("  Suggestion: Check that the file exists and is readable~n~n"),
    {error, file_read_error};
format_generic_error(FilePath, Error) ->
    io:format("~n❌ Error processing: ~s~n", [FilePath]),
    io:format("  Error: ~p~n~n", [Error]),
    {error, unknown_error}.
