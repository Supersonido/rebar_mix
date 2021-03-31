-module(rebar_mix_hex).

-behaviour(rebar_resource_v2).

-define(DEFAULT_CDN_SITE, "https://repo.hex.pm").
-define(CDN_TARBALL_LOCATION, "/tarballs").

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).

%% Init
init(Type, _State) ->
  Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
  {ok, Resource}.


%% Lock
lock(AppInfo, _) ->
  {hex, Name, Vsn} = rebar_app_info:source(AppInfo),
  {hex, rebar_mix_utils:to_binary(Name), rebar_mix_utils:to_binary(Vsn)}.


%% Download download
download(Dir, AppInfo, State, _) ->
  Pkg = rebar_app_info:source(AppInfo),
  CDN = cdn(State),
  fetch(Pkg, CDN, Dir).


%% Needs Update
needs_update(AppInfo, _) ->
  {hex, _Name, Vsn} = rebar_app_info:source(AppInfo),
  case rebar_app_info:original_vsn(AppInfo) =:= ec_cnv:to_list(Vsn) of
    true ->
      false;
    false ->
      false
  end.

%% Make VSN
make_vsn(_, _) ->
  {error, "Replacing version of type elixir not supported."}.

%%=================================
%% Private function
%%=================================
cdn(State) ->
  Opts = rebar_state:get(State, elixir_opts, []),
  CDNSite = proplists:get_value(cdn, Opts, ?DEFAULT_CDN_SITE),
  CDNSite ++ ?CDN_TARBALL_LOCATION.


fetch({hex, Name_, Vsn_}, CDN, Dir) ->
  Name = rebar_mix_utils:to_binary(Name_),
  Vsn  = rebar_mix_utils:to_binary(Vsn_),
  case filelib:is_dir(Dir) of
    true ->
      Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
      Url = string:join([CDN, Package], "/"),
      case request(Url) of
        {ok, Binary} ->
          {ok, Contents} = extract(Binary),
          ok = erl_tar:extract({binary, Contents}, [{cwd, Dir}, compressed]);
        _ ->
          rebar_api:console("Error: Unable to fetch package ~p ~p~n", [Name, Vsn])
      end;
    false ->
      ok
  end.


extract(Binary) ->
  {ok, Files} = erl_tar:extract({binary, Binary}, [memory]),
  {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
  {ok, Contents}.


request(Url) ->
  case httpc:request(get, {Url, []},
                     [{relaxed, true}],
                     [{body_format, binary}],
                     rebar) of
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} ->
      {ok, Body};
    Error ->
      Error
  end.
