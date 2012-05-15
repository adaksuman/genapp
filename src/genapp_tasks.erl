-module(genapp_tasks).

-behavior(e2_task_supervisor).

-export([start_link/0,
         start_deploy/1,
         start_deploy/2]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, {erlang, apply, []}, [registered]).

start_deploy(MetadataHome) ->
    e2_task_supervisor:start_task(
      ?MODULE, [genapp_deploy, start_link, [MetadataHome]]).

start_deploy(MetadataHome, Options) ->
    e2_task_supervisor:start_task(
      ?MODULE, [genapp_deploy, start_link, [MetadataHome, Options]]).

