-module(rebar_mix).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  %% Add elixir tu build path
  State1 = rebar_mix_utils:add_elixir(State),
  State2 = rebar_mix_utils:add_elixir_to_build_path(State1),

  %% Add rebar resources
  State3 = rebar_state:add_resource(State2, {hex, rebar_mix_hex}),
  State4 = rebar_state:add_resource(State3, {iex_dep, rebar_mix_dep}),

  %% Add project builder that works with some versions of rebar3
  State5 =
    case erlang:function_exported(rebar_state, add_project_builder, 3) of
      true -> rebar_state:add_project_builder(State4, mix, rebar_mix_compiler);
      _ -> State4
    end,

  %% Update project release config
  LibDir = rebar_mix_utils:get_lib_dir(State5),
  RelxConfig = rebar_state:get(State5, relx, []),
  NewRelxConfig =
    case lists:keyfind(lib_dirs, 1, RelxConfig) of
      {lib_dirs, OldLibDir} ->
        NewLibDir = OldLibDir ++ [LibDir],
        lists:keyreplace(lib_dirs, 1, RelxConfig, {lib_dirs, NewLibDir});
      false ->
        [{lib_dirs, [LibDir]}] ++ RelxConfig
    end,
  State6 = rebar_state:set(State5, relx, NewRelxConfig),
  rebar_mix_hook:init(State6).
