-module(genapp_event).

-behavior(e2_publisher).

-export([start_link/0,
         subscribe_app_deploy/1,
         unsubscribe_app_deploy/1,
         publish_app_deploy/1]).

start_link() ->
    e2_publisher:start_link(?MODULE, [], [registered]).

subscribe_app_deploy(Sub) ->
    e2_publisher:subscribe(?MODULE, {app_deploy, '_'}, Sub).

unsubscribe_app_deploy(Sub) ->
    e2_publisher:unsubscribe(?MODULE, {app_deploy, '_'}, Sub).

publish_app_deploy(Info) ->
    e2_publisher:publish(?MODULE, {app_deploy, Info}).
