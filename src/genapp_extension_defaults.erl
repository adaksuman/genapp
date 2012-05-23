-module(genapp_extension_defaults).

-behavior(e2_service).

-include("genapp.hrl").

-export([start_link/0, init/1, handle_msg/3]).

start_link() ->
    e2_service:start_link(?MODULE, [], []).

init([]) ->
    register_genapp_run(),
    maybe_register_sudo(),
    register_create_service_stub(),
    {ok, erlang:monitor(process, genapp_extension)}.

register_genapp_run() ->
    genapp_extension:register(run, {genapp_cmd, run}).

maybe_register_sudo() ->
    register_sudo(os:find_executable("sudo")).

register_sudo(false) -> ok;
register_sudo(_) ->
    genapp_extension:register(run_as, fun sudo/5).

sudo(User, Cmd, Args, Options, Timeout) ->
    e2_log:info({sudo, User, Cmd, Args, Options, Timeout}),
    genapp_cmd:run("sudo", ["-u", User, Cmd|Args], Options, Timeout).

register_create_service_stub() ->
    genapp_extension:register(create_service, fun create_service_stub/1).

create_service_stub(_App) -> undefined.

handle_msg({'DOWN', Ref, process, _Proc, _Reason}, _From, Ref) ->
    {stop, genapp_extension_down}.
