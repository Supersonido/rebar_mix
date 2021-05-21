-module(rebar_mix_utils).

-export([to_binary/1,
         to_string/1,
         get_lib_dir/1,
         add_elixir/1,
         create_rebar_lock_from_mix/2,
         save_rebar_lock/2,
         compile/1,
         move_to_path/3,
         elixir_to_lock/1,
         add_elixir_to_build_path/1,
         delete/1
        ]).

-ignore_xref({'Elixir.Code', eval_quoted, 2}).
-ignore_xref({'Elixir.Code', string_to_quoted, 2}).
-ignore_xref({'Elixir.Enum', to_list, 1}).
-ignore_xref({'Elixir.File', read, 1}).

%% @doc Convert binary() | list() | integer() | atom() to binary().
-spec to_binary(binary() | list() | integer() | atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).

%% @doc Convert binary() | list() | integer() | atom() to string().
-spec to_string(binary() | list() | integer() | atom()) -> string().
to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(Value) when is_list(Value) -> Value;
to_string(Value) when is_integer(Value) -> lists:flatten(io_lib:format("~p", [Value]));
to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(_) -> erlang:error(badarg).

%% @doc Gets the location of elixir libs dir.
%% 1. Try to get it from the rebar.config file.
%% 2. If it's not defined, try to find it on the system.
%% 3. If it's not defined, throws an exception.
-spec get_lib_dir(rebar_state:t()) -> string().
get_lib_dir(State) ->
  Config = rebar_state:get(State, elixir_opts, []),
  case lists:keyfind(lib_dir, 1, Config) of
    false ->
      case rebar_utils:sh("elixir -e \"IO.puts :code.lib_dir(:elixir)\"", [return_on_error]) of
        {ok, ElixirLibs_} ->
          filename:join(re:replace(ElixirLibs_, "\\s+", "", [global,{return,list}]), "../");
        _ ->
          erlang:error(missing_elixir)
      end;
    {lib_dir, Dir2} -> Dir2
  end.

%% @doc gets the actual _build/{{Profile}}/lib directory.
-spec get_build_path(rebar_state:t()) -> string().
get_build_path(State) ->
  [Profile | _] = rebar_state:current_profiles(State),
  filename:join([rebar_dir:root_dir(State), "_build/", to_string(Profile), "lib"]).

%% @doc Convert the actual mix.lock file into a list of deps in rebar.lock format.
-spec create_rebar_lock_from_mix(string(), [string()]) -> list().
create_rebar_lock_from_mix(AppDir, Deps) ->
  MixLocks = get_mix_lock(AppDir),
  lists:foldl(
    fun(AppLock, Locks) ->
        case AppLock of
          {Name, {hex, App, Version, _, _, _, _}} ->
            case lists:member(to_string(Name), Deps) of
              true ->
                Locks ++ [{to_binary(Name), {iex_dep, to_binary(App), Version}, 0}];
              false ->
                Locks
            end;
          {Name, {git, URL, _, [Ref]}} ->
            case lists:member(to_string(Name), Deps) of
              true ->
                Locks ++ [{to_binary(Name), {iex_dep, URL, Ref}, 0}];
              false ->
                Locks
            end;
          _->
            Locks
        end
    end, [], MixLocks).

%% Create a rebar.lock inside the Dir with the desps defined in Locks
-spec save_rebar_lock(string(), list()) -> ok | {error, term()}.
save_rebar_lock(Dir, Locks) ->
  rebar_config:write_lock_file(filename:join(Dir, "rebar.lock"), Locks).

%% @doc Add elixir to rebar_state
-spec add_elixir(rebar_state:t()) -> rebar_state:t().
add_elixir(State) ->
  LibDir = get_lib_dir(State),
  code:add_patha(filename:join(LibDir, "elixir/ebin")),
  code:add_patha(filename:join(LibDir, "mix/ebin")),
  code:add_patha(filename:join(LibDir, "logger/ebin")),
  State.

%% @doc Add elixir to the application build path
-spec add_elixir_to_build_path(rebar_state:t()) -> rebar_state:t().
add_elixir_to_build_path(State)->
  LibDir = get_lib_dir(State),
  BuildPath = get_build_path(State),

  %% Link Elixir
  ElixirPath = filename:join(LibDir, "elixir"),
  ElixirBuildPath = filename:join(BuildPath, "elixir"),
  file:make_symlink(ElixirPath, ElixirBuildPath),

  %% Link Logger
  LoggerPath = filename:join(LibDir, "logger"),
  LoggerBuildPath = filename:join(BuildPath, "logger"),
  file:make_symlink(LoggerPath, LoggerBuildPath),

  %% Link mix
  MixPath = filename:join(LibDir, "mix"),
  MixBuildPath = filename:join(BuildPath, "mix"),
  file:make_symlink(MixPath, MixBuildPath),

  State.

%% @doc Add elixir to a list of lock dependencies.
-spec elixir_to_lock(list()) -> nonempty_list().
elixir_to_lock(Lock) ->
  Lock ++
    [
     {<<"elixir">>, {iex_dep, <<"elixir">>, <<"">>}, 0},
     {<<"logger">>, {iex_dep, <<"logger">>, <<"">>}, 0},
     {<<"mix">>, {iex_dep, <<"mix">>, <<"">>}, 0}
    ].

%% @doc compiles a elixir app which is located in AppDir.
%% Crash compilation when something goes wrong.
-spec compile(string()) -> string().
compile(AppDir) ->
  {ok, _ } = rebar_utils:sh("mix deps.get",
                            [
                             {cd, AppDir},
                             {use_stdout, true},
                             abort_on_error,
                             {env, [
                                    {"MIX_ENV", "prod"}
                                   ]
                             }]),
  {ok, _ } = rebar_utils:sh("mix compile",
                            [
                             {cd, AppDir},
                             {use_stdout, true},
                             abort_on_error,
                             {env, [
                                    {"MIX_ENV", "prod"}
                                   ]
                             }]),
  filename:join(AppDir, "_build/prod/lib/").

%% @doc Moves a list of files located in Source to a
%% new directory located in Traget.
-spec move_to_path([string()], string(), string()) -> list().
move_to_path(Files, Source, Traget) ->
  lists:map(
    fun(File) ->
        Source1 = filename:join([Source, File]),
        Target1 = filename:join([Traget, File]),
        ec_file:copy(Source1, Target1, [recursive])
    end, Files).

-spec delete(string()) -> string().
delete(Dir) ->
  os:cmd("rm -Rf " ++ Dir).


%%=============================
%% Private functions
%%=============================

%% @doc Get deps of an app in mix.lock format
-spec get_mix_lock(string()) -> list().
get_mix_lock(AppDir) ->
  Lockfile = filename:join(AppDir, "mix.lock"),
  application:ensure_all_started(elixir),
  case 'Elixir.File':read(Lockfile) of
    {ok,Info} ->
      Opts = [{file, to_binary(Lockfile)}, {warn_on_unnecessary_quotes, false}],
      {ok, Quoted} = 'Elixir.Code':string_to_quoted(Info, Opts),
      {EvalRes, _Binding} = 'Elixir.Code':eval_quoted(Quoted, Opts),
      'Elixir.Enum':to_list(EvalRes);
    {error, _} ->
      []
  end.
