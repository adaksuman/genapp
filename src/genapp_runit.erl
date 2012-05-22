-module(genapp_runit).

-behavior(genapp_extension).

-export([init/0, run_as/5]).

init() ->
    {ok, []}.

run_as(User, Cmd, Args, Options, Timeout) ->
    todo.
