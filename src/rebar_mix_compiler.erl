-module(rebar_mix_compiler).

-export([build/1,
         format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================
build(AppInfo) ->
  AppDir = rebar_app_info:dir(AppInfo),
  BuildDir = filename:join(AppDir, "../"),
  BuildElixirDir = filename:join(AppDir, "_build/prod/lib/"),
  AppName = rebar_mix_utils:to_string(rebar_app_info:name(AppInfo)),

  rebar_mix_utils:compile(AppDir),

  {ok, Apps} = rebar_utils:list_dir(BuildElixirDir),
  Deps = Apps -- [AppName],
  rebar_mix_utils:move_to_path(Deps, BuildElixirDir, BuildDir),

  AppBuild = filename:join(AppDir, "_build/prod/lib/" ++ AppName ++ "/ebin"),
  AppTaget = filename:join(AppDir, "ebin"),
  ec_file:copy(AppBuild, AppTaget, [recursive]),

  Lock = rebar_mix_utils:create_rebar_lock_from_mix(AppDir, Deps),
  ElixirLock = rebar_mix_utils:elixir_to_lock(Lock),
  rebar_mix_utils:save_rebar_lock(AppDir, ElixirLock),
  rebar_mix_utils:delete(filename:join(AppDir, "_build")),

  ok.

format_error({mix_not_found, Name}) ->
  io_lib:format("Elixir and mix must be installed to build application ~ts. "
                "Install Elixir or check your path and try again.", [Name]);
format_error({mix_compile_failed, Name, _Error}) ->
  io_lib:format("Failed to compile application ~ts with mix", [Name]);
format_error(Reason) ->
  io_lib:format("~p", Reason).
