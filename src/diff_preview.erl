-module(diff_preview).
-export([unified/3, merge/1]).

unified(OldText, NewText, File) ->
    %% Minimal placeholder: show lengths and file name
    io_lib:format("--- ~s (old ~p chars)\n+++ ~s (new ~p chars)\n", [File, length(OldText), File, length(NewText)]).

merge(Diffs) when is_list(Diffs) ->
    string:join([iolist_to_binary(D) || D <- Diffs], "\n").
