-module(genapp_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, children(genapp:mode())}.

children(normal) ->
    [genapp_event,
     genapp_resource,
     {genapp_tasks, [supervisor]},
     genapp_user_cleanup,
     genapp_service_cleanup];
children(devmode) ->
    [genapp_event,
     genapp_resource,
     {genapp_tasks, [supervisor]}].
