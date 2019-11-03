-module(rebar_mix).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  %% Add elixir to paths
  State1 = rebar_mix_utils:add_elixir(State),
  State2 = rebar_mix_utils:add_elixir_to_build_path(State1),

  %% Add required resources
  State4 = rebar_state:add_resource(State2, {iex_dep, rebar_mix_dep}),

  %% Add compilers
  State5 = rebar_state:add_project_builder(State4, mix, rebar_mix_builder),

  %% Add hooks
  {ok, State6} = rebar_mix_hook:init(State5),
  {ok, State7} = rebar_mix_elixir_finder_hook:init(State6),

  %% Update release configuration
  LibDir = rebar_mix_utils:get_lib_dir(State5),
  RelxConfig = rebar_state:get(State7, relx, []),
  NewRelxConfig =
    case lists:keyfind(lib_dirs, 1, RelxConfig) of
      {lib_dirs, OldLibDir} ->
        NewLibDir = OldLibDir ++ [LibDir],
        lists:keyreplace(lib_dirs, 1, RelxConfig, {lib_dirs, NewLibDir});
      false ->
        [{lib_dirs, [LibDir]}] ++ RelxConfig
    end,
  State8 = rebar_state:set(State7, relx, NewRelxConfig),
  {ok, State8}.
