-module(rebar_mix_dep).

-behaviour(rebar_resource_v2).

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
  rebar_app_info:source(AppInfo).

%% Download download
download(_TmpDir, _AppInfo, _ResorceState, _State) ->
  {error, forbidden}.


%% Needs Update
needs_update(_AppInfo, _) ->
  false.


%% Make VSN
make_vsn(_, _) ->
  {error, "Replacing version of type elixir not supported."}.


%%=================================
%% Private function
%%=================================
