-module(openapi_yaml_loader).
-export([load/1, proplist_to_map/1, format_yamerl_errors/2]).

load(Path) ->
    %% Ensure yamerl application is started
    _ = ensure_started(crypto),
    _ = ensure_started(yamerl),

    BinPath = case is_binary(Path) of
        true -> binary_to_list(Path);
        false -> Path
    end,

    case file:read_file(BinPath) of
        {ok, Bin} ->
            try
                %% yamerl:decode returns a list of documents
                Docs = yamerl:decode(Bin),
                case Docs of
                    [Doc] -> {ok, proplist_to_map(Doc)};
                    _ -> {error, {yaml, multiple_documents_not_supported}}
                end
            catch
                error:{yamerl_exception, Exceptions} ->
                    %% Extract detailed error information from yamerl
                    ErrorDetails = format_yamerl_errors(Exceptions, BinPath),
                    {error, {yaml_parse_error, ErrorDetails}};
                Class:Reason:Stack ->
                    %% Generic error with stack trace
                    {error, {yaml_parse_error, #{
                        class => Class,
                        reason => Reason,
                        message => "Failed to parse YAML file",
                        file => BinPath,
                        stacktrace => Stack
                    }}}
            end;
        {error, E} -> {error, {file_read_error, E}}
    end.

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        Other -> Other
    end.

%% Convert yamerl's nested proplists to maps recursively
proplist_to_map(List) when is_list(List) ->
    case is_proplist(List) of
        true ->
            maps:from_list([{to_binary(K), proplist_to_map(V)} || {K, V} <- List]);
        false ->
            case is_string(List) of
                true -> to_binary(List);  %% Convert string to binary
                false -> [proplist_to_map(Item) || Item <- List]
            end
    end;
proplist_to_map(Other) ->
    Other.

%% Check if a list is a string (printable characters only)
%% A valid string must have at least one character and all must be printable
is_string([]) -> false;  % Empty list is an empty array, not a string
is_string([H|T]) when is_integer(H) ->
    %% Consider it a string only if it contains printable characters
    %% Printable range: space (32) to ~ (126) for ASCII, or valid Unicode
    IsPrintable = (H >= 32 andalso H =< 126) orelse (H >= 128 andalso H =< 1114111),
    IsPrintable andalso is_string_rest(T);
is_string(_) -> false.

%% Check rest of string (can include control chars like \n, \t)
is_string_rest([]) -> true;
is_string_rest([H|T]) when is_integer(H), H >= 0, H =< 1114111 ->
    is_string_rest(T);
is_string_rest(_) -> false.

is_proplist([{_K, _V} | Rest]) -> is_proplist(Rest);
is_proplist([]) -> true;
is_proplist(_) -> false.

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) ->
    case is_string(L) of
        true -> list_to_binary(L);
        false -> L  % Keep as list (it's an array, not a string)
    end;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(N) when is_integer(N) -> integer_to_binary(N);
to_binary(N) when is_float(N) -> float_to_binary(N);
to_binary(Other) -> Other.  % Keep other types as-is

%% Format yamerl exceptions into structured error details
format_yamerl_errors(Exceptions, FilePath) when is_list(Exceptions) ->
    Errors = lists:map(fun format_single_yamerl_error/1, Exceptions),
    #{
        file => FilePath,
        errors => Errors,
        message => "YAML parsing failed"
    };
format_yamerl_errors(Exception, FilePath) ->
    format_yamerl_errors([Exception], FilePath).

%% Format a single yamerl exception
format_single_yamerl_error({yamerl_parsing_error, error, Msg, Line, Col, _Token, _Type, _Extra}) ->
    #{
        type => yaml_syntax_error,
        line => Line,
        column => Col,
        message => format_yamerl_message(Msg),
        suggestion => suggest_yaml_fix(Msg)
    };
format_single_yamerl_error({yamerl_parsing_error, _Level, Msg, Line, Col, _}) ->
    #{
        type => yaml_parsing_error,
        line => Line,
        column => Col,
        message => format_yamerl_message(Msg),
        suggestion => suggest_yaml_fix(Msg)
    };
format_single_yamerl_error(Other) ->
    #{
        type => unknown_error,
        message => io_lib:format("~p", [Other]),
        suggestion => "Check YAML syntax and structure"
    }.

%% Format yamerl error message
format_yamerl_message(Msg) when is_list(Msg) ->
    Msg;
format_yamerl_message(Msg) when is_binary(Msg) ->
    binary_to_list(Msg);
format_yamerl_message(Msg) ->
    io_lib:format("~p", [Msg]).

%% Suggest fixes for common YAML errors
suggest_yaml_fix(Msg) when is_list(Msg) ->
    MsgLower = string:to_lower(Msg),
    HasIndent = string:str(MsgLower, "indent") > 0,
    HasExpect = string:str(MsgLower, "expect") > 0,
    HasFlow = string:str(MsgLower, "flow") > 0,
    HasScalar = string:str(MsgLower, "scalar") > 0,
    HasKey = string:str(MsgLower, "key") > 0,
    
    if
        HasIndent ->
            "Check YAML indentation - use consistent 2-space indents";
        HasExpect ->
            "Verify YAML syntax - check for missing colons, quotes, or proper list/map structure";
        HasFlow orelse HasScalar ->
            "Empty values are not allowed - either remove the field, use null, or provide a value like []";
        HasKey ->
            "Check for duplicate keys or keys with no values";
        true ->
            "Ensure proper YAML syntax with colons after keys and proper indentation"
    end;
suggest_yaml_fix(_) ->
    "Check YAML syntax and structure".
