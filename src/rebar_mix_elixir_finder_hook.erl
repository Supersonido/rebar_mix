-module(rebar_mix_elixir_finder_hook).

-export([init/1,
         do/1,
         format_error/1]).


-define(PROVIDER, find_elixir_libs).
-define(NAMESPACE, mix).
-define(DEPS, []).

-define(ELIXIR_CMD, "elixir -e ':code.get_path |> Enum.filter(fn(p) -> String.contains?(List.to_string(p), \"/elixir/\") end) |> Enum.map(&IO.puts/1)'").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State,
                                      providers:create([{name, ?PROVIDER},
                                                        {module, ?MODULE},
                                                        {namespace, ?NAMESPACE},
                                                        {bare, true},
                                                        {deps, ?DEPS},
                                                        {example, "rebar3 mix find_elixir_libs"},
                                                        {short_desc, "Adds elixir libs into runtime"},
                                                        {desc, "Finds and adds elixir ebin files into paths so that they are available for the runtime"},
                                                        {opts, []}])),
    {ok, State1}.

do(State) ->
    %% check if we already have elixir libs in paths
    case code:is_loaded(elixir) of
        {file, _} ->
            {ok, State};
        false ->
            %% ask elixir to print it's core libs
            case rebar_utils:sh(?ELIXIR_CMD, [{return_on_error, true}, {use_stdout, false}]) of
                {error, {127, _}} ->
                    {error, {?MODULE, elixir_not_found}};
                {error, {_Code, _Error}} ->
                    {error, {?MODULE, elixir_command_failed}};
                {ok, Output} ->
                    %% parse the output
                    Output1 = string:trim(Output),
                    LibPaths = string:split(Output1, "\n", all),
                    code:add_paths(LibPaths),
                    %% try to load elixir now
                    case code:load_file(elixir) of
                        {module, elixir} ->
                            {ok, State};
                        _Ret ->
                            {error, {?MODULE, elixir_load_error}}
                    end
            end
    end.

format_error({elixir_not_found, Name}) ->
    io_lib:format("Elixir and mix must be installed to build application ~ts. "
                  "Install Elixir or check your path and try again.", [Name]);
format_error({elixir_command_failed, Name}) ->
    io_lib:format("Elixir failed to execute command to print paths to it's library. ~ts. "
                  "Please check if command '~ts' works.", [Name, ?ELIXIR_CMD]);
format_error({elixir_load_error, Name}) ->
    io_lib:format("Elixir libraries were found but failed to load. ~ts. "
                  "Please check if Elixir is builded properly.", [Name]);

format_error(Error) ->
    io_lib:format("~p", [Error]).
