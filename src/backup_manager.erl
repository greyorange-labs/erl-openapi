-module(backup_manager).
-export([backup_many/1]).

backup_many(Paths) ->
    lists:foreach(fun backup_one/1, Paths),
    ok.

backup_one(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> file:write_file(Path ++ ".bak", Bin);
        _ -> ok
    end.
