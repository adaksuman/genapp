-module(genapp_curl).

-export([run/4]).

run(Url, LocalFile, Retries, Timeout) ->
    Args = ["-fsS",
            "-m", integer_to_list(Timeout),
            "--retry", integer_to_list(Retries),
            "-o", LocalFile,
            Url],
    handle_cmd_result(genapp_cmd:run("curl", Args)).

handle_cmd_result({0, ""}) -> ok;
handle_cmd_result({N, Err}) ->
    {error, {N, iolist_to_binary(Err)}}.
