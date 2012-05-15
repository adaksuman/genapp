-module(genapp_service_cleanup).

-behavior(e2_task).

-include("genapp.hrl").

-export([start_link/0]).

-export([init/1, handle_task/1]).

-record(state, {services_home, apps_home}).

-define(INTERVAL, 15000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    e2_task:start_link(?MODULE, []).

%%%===================================================================
%%% Init
%%%===================================================================

init([]) ->
    {ok, init_state(), timings()}.

init_state() ->
    #state{services_home=services_home(),
           apps_home=apps_home()}.

services_home() ->
    genapp:get_env(services_home, ?DEFAULT_SERVICES_HOME).

apps_home() ->
    genapp:get_env(apps_home, ?DEFAULT_APPS_HOME).

timings() ->
    {0, ?INTERVAL}.

%%%===================================================================
%%% Cleanup task
%%%===================================================================

handle_task(State) ->
    delete_services(orphaned_services(apps(State), State)),
    {repeat, State}.

apps(#state{apps_home=Dir}) ->
    handle_apps_ls(file:list_dir(Dir)).

handle_apps_ls({ok, Apps}) -> Apps;
handle_apps_ls({error, enoent}) -> [].

orphaned_services(Apps, State) ->
    AppsSet = sets:from_list(Apps),
    ServicesSet = sets:from_list(services(State)),
    sets:to_list(sets:subtract(ServicesSet, AppsSet)).

services(#state{services_home=Dir}) ->
    handle_services_ls(file:list_dir(Dir)).

handle_services_ls({ok, Services}) -> Services;
handle_services_ls({error, enoent}) -> [].

delete_services([]) -> ok;
delete_services(Services) ->
    e2_log:info({genapp_service_cleanup, Services}),
    lists:foreach(fun genapp_service:delete/1, Services).
