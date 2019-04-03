-module(rebar_mix).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_mix_hook:init(State),
    {ok, State2} = rebar_mix_elixir_finder_hook:init(State1),
    State3 = rebar_state:add_project_builder(State2, mix, rebar_mix_builder),
    rebar_mix_elixir_finder_hook:process_elixir_lib_paths(State3).
