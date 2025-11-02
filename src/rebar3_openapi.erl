-module(rebar3_openapi).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_openapi_gen_erlang_prv:init(State),
    {ok, State2} = rebar3_openapi_gen_spec_prv:init(State1),
    {ok, State2}.

