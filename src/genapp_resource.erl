%%%===================================================================
%%% @module
%%% Provides synchronizes management of various app resources including
%%% application directories and port reservation.
%%% @end
%%%===================================================================

-module(genapp_resource).

-behavior(e2_service).

-include("genapp.hrl").

-export([start_link/0,
         create_new_app_dir/0,
         create_new_app_dir/1,
         apps/0,
         app_dir/1,
         reserve_app_ports/2,
	 query_app/2,
	 query_apps/1,
	 delete_app_dir/1,
	 app_exists/1]).

-export([init/1, handle_msg/3]).

-record(state, {home, port_min, port_max}).

-define(DEFAULT_PORT_MIN, 8000).
-define(DEFAULT_PORT_MAX, 8999).
-define(CREATE_APP_DIR_ATTEMPTS, 3).
-define(RM_APP_DIR_TIMEOUT, 30000).
-define(TAIL_TIMEOUT, 30000).

-define(valid_range(Min, Max),
        (is_integer(Min) andalso
         is_integer(Max) andalso
         Min >= 1024 andalso
         Max >= Min andalso
         Max < 65534)).

-define(valid_appid(Id), length(Id) > 0).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

create_new_app_dir() ->
    e2_service:call(?MODULE, new_app_dir).

create_new_app_dir(AppId) ->
    e2_service:call(?MODULE, {new_app_dir, AppId}).

reserve_app_ports(#app{id=Id}, Count) ->
    e2_service:call(?MODULE, {reserve_ports, Id, Count});
reserve_app_ports(AppId, Count) ->
    e2_service:call(?MODULE, {reserve_ports, AppId, Count}).

apps() ->
    e2_service:call(?MODULE, apps).

app_dir(AppId) ->
    e2_service:call(?MODULE, {app_dir, AppId}).

-type query_option() :: ports | metadata | setup_status.
-type app_id() :: string().
-type query_result() :: {app_id(), [{query_option(), [term()]}]}.
-spec query_apps([query_option()]) -> [query_result()].

query_apps(Options) ->
    e2_service:call(?MODULE, {query_apps, Options}).

-spec query_app(app_id(), [query_option()]) -> [query_result()].

query_app(AppId, Options) when ?valid_appid(AppId) ->
    e2_service:call(?MODULE, {query_app, AppId, Options}).

delete_app_dir(AppId) when ?valid_appid(AppId) ->
    e2_service:call(?MODULE, {delete_app_dir, AppId}).

app_exists(AppId) when ?valid_appid(AppId) ->
    e2_service:call(?MODULE, {app_exists, AppId}).

%%%===================================================================
%%% Init
%%%===================================================================

init([]) ->
    {PortMin, PortMax} = port_range(),
    {ok, #state{home=apps_home(),
                port_min=PortMin,
                port_max=PortMax}}.

port_range() ->
    Min = genapp:get_env(app_port_min, ?DEFAULT_PORT_MIN),
    Max = genapp:get_env(app_port_max, ?DEFAULT_PORT_MAX),
    validate_port_range(Min, Max),
    {Min, Max}.

validate_port_range(Min, Max) when ?valid_range(Min, Max) -> ok;
validate_port_range(Min, Max) -> exit({invalid_port_range, {Min, Max}}).

apps_home() ->
    validate_apps_home(genapp:get_env(apps_home, ?DEFAULT_APPS_HOME)).

validate_apps_home(Dir) ->
    handle_required_dir(filelib:is_dir(Dir), Dir).

handle_required_dir(true, Dir) -> Dir;
handle_required_dir(false, Dir) -> exit({missing_required_dir, Dir}).

%%%===================================================================
%%% Message dispatch
%%%===================================================================

handle_msg(new_app_dir, _From, State) ->
    {reply, new_app_dir(State), State};
handle_msg({new_app_dir, AppId}, _From, State) ->
    {reply, new_app_dir(AppId, State), State};
handle_msg({reserve_ports, AppId, Count}, _From, State) ->
    {reply, reserve_ports(AppId, Count, State), State};
handle_msg(apps, _From, State) ->
    {reply, apps(State), State};
handle_msg({app_dir, AppId}, _From, State) ->
    {reply, check_app_dir(AppId, State), State};
handle_msg({query_apps, Options}, _From, State) ->
    {reply, query_apps_(Options, State), State};
handle_msg({query_app, Id, Options}, _From, State) ->
    {reply, query_app_(Id, Options, State), State};
handle_msg({delete_app_dir, Id}, _From, State) ->
    {reply, delete_app(Id, State), State};
handle_msg({app_exists, Id}, _From, State) ->
    {reply, app_exists(Id, State), State}.

%%%===================================================================
%%% New app dir
%%%===================================================================

new_app_dir(Id, #state{home=Home}) ->
    AppDir = filename:join(Home, Id),
    handle_app_dir_create(file:make_dir(AppDir), {Id, AppDir}).

handle_app_dir_create(ok, {Id, AppDir}) -> {Id, AppDir};
handle_app_dir_create({error, Err}, {_, AppDir}) ->
    error({create_app_dir, AppDir, Err}).

new_app_dir(#state{home=Home}) ->
    try_new_app_dir(Home, ?CREATE_APP_DIR_ATTEMPTS).

try_new_app_dir(_Home, 0) -> exit(too_many_create_app_dir_attempts);
try_new_app_dir(Home, Attempts) ->
    Id = new_app_id(),
    AppDir = filename:join(Home, Id),
    handle_app_dir_attempt(
      file:make_dir(AppDir), {Id, AppDir}, Home, Attempts).

new_app_id() ->
    NowBin = term_to_binary(erlang:now()),
    lists:sublist(genapp_util:bin_to_hex(erlang:md5(NowBin)), 8).

handle_app_dir_attempt(ok, {Id, AppDir}, _Home, _Attempts) ->
    {Id, AppDir};
handle_app_dir_attempt({error, eexists}, _, Home, Attempts) ->
    try_new_app_dir(Home, Attempts - 1).

%%%===================================================================
%%% Reserve ports
%%%===================================================================

reserve_ports(AppId, Count, State) ->
    handle_valid_app_reserve_ports(check_app_dir(AppId, State), Count, State).

handle_valid_app_reserve_ports({ok, AppDir}, Count, State) ->
    handle_get_free_ports(get_free_ports(Count, State), AppDir);
handle_valid_app_reserve_ports(error, _Count, _State) ->
    {error, invalid_app}.

get_free_ports(Count, State) ->
    get_free_ports(Count, reserved_ports(State), ports_range(State), []).

ports_range(#state{port_min=Min, port_max=Max}) ->
    {Min, Max}.

reserved_ports(State) ->
    acc_ports_set(apps(State), State, sets:new()).

apps(#state{home=Home}) ->
    {ok, Apps} = file:list_dir(Home),
    Apps.

acc_ports_set([], _, Ports) -> Ports;
acc_ports_set([App|Rest], State, Ports) ->
    acc_ports_set(Rest, State, add_app_ports(App, State, Ports)).

add_app_ports(App, State, PortsAcc) ->
    lists:foldl(
      fun(Port, Set) -> sets:add_element(Port, Set) end,
      PortsAcc, app_ports(App, State)).

handle_get_free_ports({error, Err}, _AppDir) ->
    {error, Err};
handle_get_free_ports({ok, Ports}, AppDir) ->
    write_ports(Ports, AppDir),
    {ok, Ports}.

get_free_ports(0, _Reserved, _Range, Acc) -> {ok, Acc};
get_free_ports(N, Reserved, Range, Acc) ->
    case get_free_port(Reserved, Range) of
        {ok, Port} ->
            get_free_ports(
              N - 1, add_reserved(Port, Reserved), Range, [Port|Acc]);
        no_ports ->
            {error, no_ports}
    end.

get_free_port(Reserved, Range) ->
    find_next_port(rand_port(Range), 0, Reserved, Range).

find_next_port(Seed, Offset, Reserved, Range) ->
    find_next_port(
      port_available(Seed - Offset, Reserved, Range),
      port_available(Seed + Offset, Reserved, Range),
      Seed, Offset, Reserved, Range).

find_next_port(out_of_range, out_of_range, _, _, _, _) ->
    no_ports;
find_next_port({true, Port}, _, _, _, _, _) ->
    {ok, Port};
find_next_port(_, {true, Port}, _, _, _, _) ->
    {ok, Port};
find_next_port(_, _, Seed, Offset, Reserved, Range) ->
    find_next_port(Seed, Offset + 1, Reserved, Range).

rand_port({Min, Max}) ->
    Min + erlang:phash2(erlang:now(), (Max - Min)).

port_available(Port, Reserved, {Min, Max})
  when Port >= Min, Port =< Max ->
    case is_reserved(Port, Reserved) of
        true -> false;
        false -> {true, Port}
    end;
port_available(_, _, _) -> out_of_range.

add_reserved(Port, Reserved) ->
    sets:add_element(Port, Reserved).

is_reserved(Port, Reserved) ->
    sets:is_element(Port, Reserved).

write_ports(Ports, AppDir) ->
    lists:foreach(fun(Port) -> write_port(Port, AppDir) end, Ports).

write_port(Port, AppDir) ->
    File = filename:join([AppDir, ?GENAPP_SUBDIR, ?GENAPP_PORTS_SUBDIR,
                          integer_to_list(Port)]),
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, <<>>).

%%%===================================================================
%%% Query apps
%%%===================================================================

query_app_(AppId, Options, State) ->
    handle_query_app_dir(check_app_dir(AppId, State), Options).

handle_query_app_dir({ok, AppDir}, Options) ->
    {ok, query_app_(AppDir, Options)};
handle_query_app_dir(error, _Options) -> error.

query_apps_(Options, State) ->
    query_apps_(app_dirs(State), Options, []).

query_apps_([], _Options, Acc) -> Acc;
query_apps_([{Id, Dir}|Rest], Options, Acc) ->
    query_apps_(Rest, Options, [{Id, query_app_(Dir, Options)}|Acc]).

query_app_(Dir, Options) ->
    query_app_acc(Dir, Options, []).

query_app_acc(_Dir, [], Acc) -> Acc;
query_app_acc(Dir, [ports|Rest], Acc) ->
    query_app_acc(Dir, Rest, [{ports, app_ports(Dir)}|Acc]);
query_app_acc(Dir, [metadata|Rest], Acc) ->
    query_app_acc(Dir, Rest, [{metadata, app_metadata(Dir)}|Acc]);
query_app_acc(Dir, [setup_status|Rest], Acc) ->
    query_app_acc(Dir, Rest, [{setup_status, app_setup_status(Dir)}|Acc]);
query_app_acc(Dir, [setup_logs|Rest], Acc) ->
    query_app_acc(Dir, Rest, [{setup_logs, app_setup_logs(Dir)}|Acc]);
query_app_acc(Dir, [{log_tail, Bytes}|Rest], Acc) ->
    query_app_acc(Dir, Rest, [{log_tail, log_tail(Dir, Bytes)}|Acc]);
query_app_acc(Dir, [MetaFile|Rest], Acc) ->
    query_app_acc(Dir, Rest, [{MetaFile, meta_file(Dir, MetaFile)}|Acc]).

%%%===================================================================
%%% List ports
%%%===================================================================

app_ports(AppId, State) ->
    app_ports(app_dir(AppId, State)).

app_ports(AppDir) ->
    handle_ports_dir_list(file:list_dir(ports_dir(AppDir)), []).

ports_dir(AppDir) ->
    filename:join([AppDir, ?GENAPP_SUBDIR, ?GENAPP_PORTS_SUBDIR]).

handle_ports_dir_list({ok, Dirs}, Ports) ->
    lists:foldl(
      fun(PortStr, Acc) -> add_port_as_int(PortStr, Acc) end,
      Ports, Dirs);
handle_ports_dir_list({error, enoent}, Ports) -> Ports.

add_port_as_int(PortStr, Ports) ->
    handle_port_to_int(catch(list_to_integer(PortStr)), Ports).

handle_port_to_int(Port, Ports) when is_integer(Port) -> [Port|Ports];
handle_port_to_int(_, Ports) -> Ports.

%%%===================================================================
%%% App metadata
%%%===================================================================

app_metadata(AppDir) ->
    MetaFile = app_metadata_file(AppDir),
    handle_read_app_metadata(file:read_file(MetaFile)).

app_metadata_file(AppDir) ->
    filename:join([AppDir, ?GENAPP_SUBDIR, ?GENAPP_METADATA_FILE]).

handle_read_app_metadata({ok, Bin}) -> Bin;
handle_read_app_metadata({error, Err}) -> {error, Err}.

%%%===================================================================
%%% App setup status and logs
%%%===================================================================

app_setup_status(AppDir) ->
    app_setup_status(setup_status_dir(AppDir), ?SETUP_STATUS).

app_setup_logs(AppDir) ->
    read_setup_logs(plugin_setup_log_files(AppDir)).

setup_status_dir(AppDir) ->
    filename:join([AppDir, ?GENAPP_SUBDIR, ?GENAPP_SETUP_STATUS_SUBDIR]).

app_setup_status(_Dir, []) -> unknown;
app_setup_status(Dir, [Status|Rest]) ->
    handle_check_setup_status(
      filelib:is_file(setup_status_file(Dir, Status)),
      Status, Dir, Rest).

setup_status_file(Dir, Status) ->
    filename:join(Dir, atom_to_list(Status)).

handle_check_setup_status(true, Status, _Dir, _Rest) -> Status;
handle_check_setup_status(false, _Status, Dir, Rest) ->
    app_setup_status(Dir, Rest).

plugin_setup_log_files(AppDir) ->
    filelib:wildcard(filename:join(setup_status_dir(AppDir), "plugin_*")).

read_setup_logs(Logs) ->
    read_setup_logs_acc(Logs, []).

read_setup_logs_acc([], Acc) -> Acc;
read_setup_logs_acc([File|Rest], Acc) ->
    LogInfo = split_plugin_log_name(File),
    read_setup_logs_acc(
      Rest, handle_log_read(file:read_file(File), LogInfo, Acc)).

split_plugin_log_name(File) ->
    Base = filename:basename(File),
    handle_plugin_log_name_split(
      re:run(Base, "plugin_(.*?)_(\\d+)$", [{capture, all_but_first, list}])).

handle_plugin_log_name_split({match, [Plugin, ExitStr]}) ->
    {Plugin, list_to_integer(ExitStr)};
handle_plugin_log_name_split(nomatch) -> error.

handle_log_read({ok, Bin}, {Plugin, 0}, Acc) ->
    [{Plugin, {ok, Bin}}|Acc];
handle_log_read({ok, Bin}, {Plugin, N}, Acc) ->
    [{Plugin, {error, {N, Bin}}}|Acc];
handle_log_read(_, _, Acc) -> Acc.

%%%===================================================================
%%% Delete app
%%%===================================================================

delete_app(Id, State) ->
    handle_delete_valid_app(check_app_dir(Id, State)).

handle_delete_valid_app({ok, Dir}) ->
    validate_app_dir(Dir),
    handle_rm_dir(genapp_cmd:run("rm", ["-rf", Dir], [], ?RM_APP_DIR_TIMEOUT));
handle_delete_valid_app(error) -> error.

validate_app_dir(Dir) ->
    validate_app_dir_parts(filename:split(Dir)).

validate_app_dir_parts(Parts) when length(Parts) < 3 ->
    error(invalid_app);
validate_app_dir_parts(_Parts) -> ok.

handle_rm_dir({0, _}) -> ok;
handle_rm_dir({error, Err}) -> {error, Err};
handle_rm_dir({N, Err}) -> {error, {N, Err}}.

%%%===================================================================
%%% Log tail
%%%===================================================================

log_tail(AppDir, Bytes) ->
    tail(current_log(AppDir), Bytes).

current_log(AppDir) ->
    filename:join([AppDir, ?GENAPP_SUBDIR, ?GENAPP_LOG_SUBDIR, "current"]).

tail(File, Bytes) when is_integer(Bytes), Bytes >= 0 ->
    handle_tail_result(
      genapp_cmd:run(
        "tail", ["-c", integer_to_list(Bytes), File], [], ?TAIL_TIMEOUT));
tail(_File, Bytes) ->
    {error, {invalid_log_size, Bytes}}.

handle_tail_result({0, Out}) -> {ok, Out};
handle_tail_result({1, _}) -> {ok, ""};
handle_tail_result({N, Err}) -> {error, {N, Err}}.

%%%===================================================================
%%% App meta file
%%%===================================================================

meta_file(Dir, MetaFile) ->
    read_meta_file(meta_file_name(Dir, MetaFile)).

meta_file_name(AppDir, MetaFile) ->
    filename:join([AppDir, ?GENAPP_SUBDIR, MetaFile]).

read_meta_file(File) ->
    handle_meta_file_read(file:read_file(File)).

handle_meta_file_read({ok, Bin}) -> Bin;
handle_meta_file_read({error, enoent}) -> error;
handle_meta_file_read({error, enotdir}) -> error.

%%%===================================================================
%%% Misc
%%%===================================================================

app_exists(AppId, State) ->
    filelib:is_dir(app_dir(AppId, State)).

app_dir(AppId, #state{home=Home}) ->
    filename:join(Home, AppId).

app_dirs(#state{home=Home}) ->
    {ok, Apps} = file:list_dir(Home),
    [{Id, filename:join(Home, Id)} || Id <- Apps].

check_app_dir(AppId, State) ->
    AppDir = app_dir(AppId, State),
    handle_check_app_dir(filelib:is_dir(AppDir), AppDir).

handle_check_app_dir(true, AppDir) -> {ok, AppDir};
handle_check_app_dir(false, _) -> error.
