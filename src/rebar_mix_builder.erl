-module(rebar_mix_builder).

-export([build/1,
         format_error/1,
         sh/2]).

build(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    case sh("elixir -pa \"../*/ebin\" -S mix compile --no-load-deps "
                        "--no-deps-check --no-protocol-consolidation",
                        [{cd, AppDir},
                         {return_on_error, true},
                         {use_stdout, true},
                         {env, [{"MIX_BUILD_PATH", filename:join(AppDir, "../../")},
                                {"MIX_ENV", "prod"}]}]) of
        {error, {127, _}} ->
            {error, {mix_not_found, rebar_app_info:name(AppInfo)}};
        {error, {_Code, Error}} ->
            {error, {mix_compile_failed, rebar_app_info:name(AppInfo), Error}};
        _ ->
            ok
    end.

format_error({mix_not_found, Name}) ->
    io_lib:format("Elixir and mix must be installed to build application ~ts. "
                  "Install Elixir or check your path and try again.", [Name]);
format_error({mix_compile_failed, Name, _Error}) ->
    io_lib:format("Failed to compile application ~ts with mix", [Name]);
format_error(Reason) ->
    io_lib:format("~p", Reason).

sh(Command, Options) ->
    rebar_utils:sh(Command, [{env, [{"ERL_FLAGS", ""}]} | Options]).