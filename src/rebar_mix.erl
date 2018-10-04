-module(rebar_mix).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_mix_hook:init(State),
    State2 = rebar_state:add_project_builder(State1, mix, rebar_mix_builder),
    {ok, State2}.
