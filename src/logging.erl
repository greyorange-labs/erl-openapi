-module(logging).
-export([info/2, warn/2, error/2]).

info(Fmt, Args) -> io:format("[INFO] " ++ Fmt ++ "\n", Args).
warn(Fmt, Args) -> io:format("[WARN] " ++ Fmt ++ "\n", Args).
error(Fmt, Args) -> io:format("[ERROR] " ++ Fmt ++ "\n", Args).
