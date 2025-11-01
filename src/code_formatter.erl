-module(code_formatter).
-export([format_file/1]).

%% Format an Erlang file using rebar3 fmt command
format_file(FilePath) ->
    case filelib:is_file(FilePath) of
        true ->
            %% Use absolute path
            AbsPath = filename:absname(FilePath),

            %% Find rebar3 executable
            Rebar3 = find_rebar3(),

            %% Build the command
            Cmd = io_lib:format("~s fmt -w '~s' 2>&1", [Rebar3, AbsPath]),
            CmdStr = lists:flatten(Cmd),

            %% Execute the command
            Output = os:cmd(CmdStr),

            %% Check for errors (empty output or just whitespace means success)
            Trimmed = string:trim(Output),
            case Trimmed of
                "" ->
                    io:format("Formatted: ~s~n", [FilePath]),
                    ok;
                _ ->
                    case string:str(Output, "Error") > 0 orelse string:str(Output, "not found") > 0 of
                        true ->
                            io:format("Warning: Failed to format ~s~nOutput: ~s~n", [FilePath, Output]),
                            ok;
                        false ->
                            %% Some output but not an error, probably success
                            io:format("Formatted: ~s~n", [FilePath]),
                            ok
                    end
            end;
        false ->
            {error, enoent}
    end.

%% Find rebar3 executable in common locations
find_rebar3() ->
    %% Try os:find_executable first
    case os:find_executable("rebar3") of
        false ->
            %% Try common paths
            CommonPaths = [
                filename:join([os:getenv("HOME"), ".cache", "rebar3", "bin", "rebar3"]),
                "/usr/local/bin/rebar3",
                "./rebar3"
            ],
            case lists:filter(fun filelib:is_file/1, CommonPaths) of
                [Path | _] -> Path;
                [] -> "rebar3"  %% Fallback, hope it's in PATH
            end;
        Path -> Path
    end.

%% Find project root by looking for rebar.config (currently unused but kept for future use)
%% find_project_root(FilePath) ->
%%     Dir = filename:dirname(FilePath),
%%     case filelib:is_file(filename:join(Dir, "rebar.config")) of
%%         true -> Dir;
%%         false ->
%%             Parent = filename:dirname(Dir),
%%             case Parent of
%%                 Dir -> Dir;  %% Reached filesystem root
%%                 _ -> find_project_root(Parent)
%%             end
%%     end.

