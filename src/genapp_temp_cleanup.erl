-module(genapp_temp_cleanup).

-behavior(e2_task).

-include("genapp.hrl").

-include_lib("kernel/include/file.hrl").

-export([start_link/0, run_once/0]).

-export([handle_task/1]).

-define(INTERVAL, 15 * 60000).
-define(OLD_REMOTE_PLUGIN_MINUTES, 60).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    e2_task:start_link(?MODULE, [], [registered, {repeat, ?INTERVAL}]).

run_once() ->
    e2_task:run_once(?MODULE).

%%%===================================================================
%%% Task
%%%===================================================================

handle_task(State) ->
    delete_old_remote_plugins(),
    {repeat, State}.

delete_old_remote_plugins() ->
    delete_remote_plugins(filter_old_remote_plugins(list_remote_plugins())).

list_remote_plugins() ->
    filelib:wildcard(
      genapp_util:filename_join(
        temp_dir(), ?REMOTE_PLUGIN_TEMP_PREFIX ++ "*")).

filter_old_remote_plugins(Dirs) ->
    Cutoff = old_remote_plugin_cutoff(),
    lists:filter(fun(Dir) -> is_old_remote_plugin(Dir, Cutoff) end, Dirs).

old_remote_plugin_cutoff() ->
    now_seconds() - ?OLD_REMOTE_PLUGIN_MINUTES * 60.

now_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

is_old_remote_plugin(Dir, Cutoff) ->
    handle_old_remote_plugin_info(file:read_file_info(Dir), Cutoff).

handle_old_remote_plugin_info({ok, Info}, Cutoff) ->
    file_ctime_seconds(Info) < Cutoff;
handle_old_remote_plugin_info({error, _}, _Cutoff) -> false.

file_ctime_seconds(#file_info{ctime=CTime}) ->
    calendar:datetime_to_gregorian_seconds(CTime).

delete_remote_plugins([]) -> ok;
delete_remote_plugins(Dirs) ->
    e2_log:info({genapp_temp_cleanup, Dirs}),
    lists:foreach(fun delete_dir/1, Dirs).

delete_dir(Dir) ->
    validate_dir(Dir),
    {0, ""} = genapp_cmd:run("rm", ["-rf", Dir]).

validate_dir(Dir) ->
    handle_validate_dir_parts(filename:split(Dir), Dir).

handle_validate_dir_parts(Parts, Dir) when length(Parts) < 3 ->
    error({invalid_dir, Dir});
handle_validate_dir_parts(_Parts, _Dir) -> ok.

%%%===================================================================
%%% Misc functions
%%%===================================================================

temp_dir() ->
    genapp:get_env(tmp_dir, ?DEFAULT_TEMP_DIR).
