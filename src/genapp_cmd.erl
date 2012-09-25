-module(genapp_cmd).

-export([run/2, run/3, run/4]).

run(Exe, Args) ->
    run(Exe, Args, [], infinity).

run(Exe, Args, Options) ->
    run(Exe, Args, clean_options(Options), infinity).

run(Exe, Args, Options, Timeout) ->
    Self = self(),
    FullExe = find_exe(Exe),
    spawn(fun() ->
		  start_port(FullExe, Args, clean_options(Options), Self)
	  end),
    receive
        {error, Err} -> error(Err);
        {N, Out} when is_integer(N) -> {N, Out}
    after
        Timeout -> error(timeout)
    end.

clean_options(Options) ->
    clean_env_option(Options).

clean_env_option(Options) ->
    handle_env_option(proplists:get_value(env, Options), Options).

handle_env_option(undefined, Options) -> Options;
handle_env_option(Env, Options) ->
    [{env, env_to_strings(Env)}|proplists:delete(env, Options)].

env_to_strings(Env) ->
    [{to_string(Name), to_string(Val)} || {Name, Val} <- Env].

to_string(L) when is_list(L) -> L;
to_string(B) when is_binary(B) -> binary_to_list(B).

find_exe(Exe) ->
    handle_exe_is_file(filelib:is_file(Exe), Exe).

handle_exe_is_file(true, Exe) -> Exe;
handle_exe_is_file(false, Exe) ->
    handle_os_find_exe(os:find_executable(Exe), Exe).

handle_os_find_exe(false, Exe) -> error({bad_exe, Exe});
handle_os_find_exe(FullExe, _Exe) -> FullExe.

start_port(Exe, Args, Options, From) ->
    try open_port({spawn_executable, Exe},
                  [{args, Args},
                   stderr_to_stdout,
                   exit_status|
                   Options]) of
        Port -> port_loop(Port, From, [])
    catch
        error:Err -> From ! {error, Err}
    end.

port_loop(Port, From, AccOut) ->
    receive
	{Port, {data, Data}} ->
	    port_loop(Port, From, [Data|AccOut]);
	{Port, {exit_status, Exit}} ->
            From ! {Exit, lists:reverse(AccOut)}
    end.
