-module(genapp_cmd).

-export([run/2, run/3, run/4]).

run(Exe, Args) ->
    run(Exe, Args, [], infinity).

run(Exe, Args, Options) ->
    run(Exe, Args, Options, infinity).

run(Exe, Args, Options, Timeout) ->
    Self = self(),
    FullExe = find_exe(Exe),
    spawn(fun() -> start_port(FullExe, Args, Options, Self) end),
    receive
        {error, Err} -> error(Err);
        {N, Out} when is_integer(N) -> {N, Out}
    after
        Timeout -> {error, timeout}
    end.

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
