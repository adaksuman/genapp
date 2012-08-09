-module(genapp_event).

-behavior(e2_publisher).

-export([start_link/0,
         subscribe_app_deploy/2,
         unsubscribe_app_deploy/2,
         publish_app_deploy/2]).

start_link() ->
    e2_publisher:start_link(?MODULE, [], [registered]).

subscribe_app_deploy(Origin, Subscriber) ->
    e2_publisher:subscribe(?MODULE, {app_deploy, Origin, '_'}, Subscriber).

unsubscribe_app_deploy(Origin, Subscriber) ->
    e2_publisher:unsubscribe(?MODULE, {app_deploy, Origin, '_'}, Subscriber).

publish_app_deploy(Origin, Event) ->
    e2_publisher:publish(?MODULE, {app_deploy, Origin, Event}).
