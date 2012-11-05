-module(genapp_deploy).

-behavior(e2_task).

-include("genapp.hrl").

-export([start_link/1, start_link/2]).

-export([init/1, handle_task/1]).

-record(state, {app, plugins, notify}).
-record(plugin, {dir, name}).

-define(UNZIP_TIMEOUT_SECONDS, 300).

%%%===================================================================
%%% API
%%%===================================================================

start_link(PkgDir) ->
    start_link(PkgDir, []).

start_link(PkgDir, Options) ->
    e2_task:start_link(?MODULE, [PkgDir, Options]).

%%%===================================================================
%%% Init
%%%===================================================================

init([PkgDir, Options]) ->
    {ok, #state{app=new_app(PkgDir, Options),
		notify=notify_option(Options)}}.

new_app(PkgDir, Options) ->
    Meta = genapp_metadata:parse_file(metadata_file(PkgDir)),
    #app{id=app_id_option(Options), meta=Meta, pkg_dir=PkgDir}.

metadata_file(PkgDir) ->
    genapp_util:filename_join([PkgDir, ?GENAPP_SUBDIR, ?GENAPP_METADATA_FILE]).

app_id_option(Options) ->
    proplists:get_value(id, Options).

notify_option(Options) ->
    proplists:get_value(notify, Options).

%%%===================================================================
%%% Deploy task
%%%===================================================================

handle_task(State) ->
    apply_stages(
      [resolve_plugins,
       reservation,
       env,
       setup,
       {setup_status, ok}],
      State),
    {stop, normal}.

apply_stages([], State) -> State;
apply_stages([Stage|Rest], State) ->
    handle_stage_result(apply_stage(Stage, State), Stage, Rest, State).

handle_stage_result(State, Stage, Rest, _State0) ->
    notify_stage_completed(Stage, State),
    apply_stages(Rest, State).

notify_stage_completed(_Stage, #state{notify=undefined}) -> ok;
notify_stage_completed(Stage, #state{notify=Notify}=State) ->
    apply_notify(Notify, stage_completed_event(Stage, State)).

stage_completed_event(Stage, #state{app=App}) ->
    {genapp_deploy, self(), Stage, App}.

apply_notify(Fun, Event) when is_function(Fun) -> Fun(Event);
apply_notify({M, F, A}, Event) -> erlang:apply(M, F, A ++ [Event]);
apply_notify(Pid, Event) when is_pid(Pid) -> erlang:send(Pid, Event).

%%%===================================================================
%%% Stage dispatch
%%%===================================================================

apply_stage(resolve_plugins, State) ->
    apply_stages(
      [{plugin, Plugin} || Plugin <- app_plugins(State)],
      State);
apply_stage({plugin, Plugin}, State) ->
    resolve_plugin(Plugin, State);
apply_stage(reservation, State) ->
    apply_stages(
      [app_directory, {setup_status, pending}, app_user, ports],
      State);
apply_stage(env, State) ->
    write_env(State);
apply_stage(setup, State) ->
    apply_stages(
      [pre_setup, plugins_setup, post_setup, {extension_notify, setup}],
      State);
apply_stage(app_directory, State) ->
    create_app_directory(State);
apply_stage(app_user, State) ->
    create_app_user(State);
apply_stage(ports, State) ->
    reserve_ports(State);
apply_stage(pre_setup, State) ->
    pre_setup(State);
apply_stage(plugins_setup, State) ->
    plugins_setup(State);
apply_stage(post_setup, State) ->
    post_setup(State);
apply_stage(app_dir_skel, State) ->
    create_app_dir_skel(State);
apply_stage(app_metadata, State) ->
    write_app_metadata(State);
apply_stage(setup_script, State) ->
    write_app_setup_script(State);
apply_stage({setup_status, Status}, State) ->
    setup_status(Status, State);
apply_stage({extension_notify, Event}, State) ->
    extension_notify_app_event(Event, State).

%%%===================================================================
%%% Resolve plugins
%%%===================================================================

app_plugins(#state{app=App}) ->
    app_plugins(App);
app_plugins(#app{meta=Meta}) ->
    validate_plugins_count(plugins_from_metadata(Meta)).

validate_plugins_count([]) -> exit(missing_app_plugins);
validate_plugins_count(Plugins) -> Plugins.

plugins_from_metadata(Meta) ->
    handle_missing_plugins_metadata(
      genapp_metadata:get_value(<<"app">>, <<"plugins">>, Meta)).

handle_missing_plugins_metadata(error) -> [];
handle_missing_plugins_metadata({ok, Plugins}) -> Plugins.

resolve_plugin(Plugin, State) ->
    add_plugin(resolve_plugin(Plugin), State).

resolve_plugin(Name) when is_binary(Name) ->
    handle_local_plugin_dir(genapp_plugin:local_plugin_dir(Name), Name);
resolve_plugin({Attrs}) ->
    resolve_plugin_src(plugin_src(Attrs), Attrs).

handle_local_plugin_dir({ok, Dir}, Name) ->
    #plugin{dir=Dir, name=Name};
handle_local_plugin_dir(error, Name) ->
    exit({plugin_not_installed, Name}).

plugin_src(Attrs) ->
    proplists:get_value(<<"src">>, Attrs).

resolve_plugin_src(undefined, Attrs) ->
    exit({missing_plugin_src, Attrs});
resolve_plugin_src(Src, Attrs) ->
    {Scheme, Rest} = parse_plugin_src(Src),
    resolve_plugin_src(Scheme, Rest, Attrs).

parse_plugin_src(Src) ->
    handle_plugin_src_match(
      re:run(Src, <<"([^:]+)://(.+)">>, [{capture, all_but_first, binary}]),
      Src).

handle_plugin_src_match(nomatch, Src) ->
    exit({invalid_plugin_src, Src});
handle_plugin_src_match({match, [Scheme, Rest]}, _Src) ->
    {Scheme, Rest}.

-define(is_curl_scheme(S), (S == <<"file">> orelse
                            S == <<"http">> orelse
                            S == <<"https">> orelse
                            S == <<"ftp">>)).

resolve_plugin_src(<<"local_plugin">>, PluginDir, Attrs) ->
    resolve_local_plugin_dir(
      filelib:is_dir(PluginDir),
      PluginDir,
      local_plugin_name(Attrs, PluginDir));
resolve_plugin_src(Scheme, Rest, Attrs) when ?is_curl_scheme(Scheme) ->
    resolve_remote_plugin(Scheme, Rest, Attrs);
resolve_plugin_src(Scheme, _Rest, _Attrs) ->
    error({invalid_plugin_src_scheme, Scheme}).

%%%===================================================================
%%% Local plugin resolution
%%%===================================================================

local_plugin_name(Attrs, Dir) ->
    attr_or_dir_name(proplists:get_value(<<"name">>, Attrs), Dir).

attr_or_dir_name(undefined, Dir) -> filename:basename(Dir);
attr_or_dir_name(Name, _Dir) -> Name.

resolve_local_plugin_dir(false, Dir, _Name) ->
    exit({invalid_local_plugin_dir, Dir});
resolve_local_plugin_dir(true, Dir, Name) ->
    #plugin{dir=Dir, name=Name}.

%%%===================================================================
%%% Remote plugin resolution
%%%===================================================================

resolve_remote_plugin(Scheme, Rest, Attrs) ->
    resolve_cached_plugin(plugin_src_hash(Attrs), Scheme, Rest, Attrs).

plugin_src_hash(Attrs) ->
    proplists:get_value(<<"sha1">>, Attrs).

resolve_cached_plugin(Hash, Scheme, Rest, Attrs) ->
    e2_log:info("TODO: use hash ~p to maybe use a cached plugin~n", [Hash]),
    install_temp_remote_plugin(
      remote_plugin_name(Attrs), remote_url(Scheme, Rest)).

remote_url(Scheme, Rest) ->
    <<Scheme/binary, (<<"://">>)/binary, Rest/binary>>.

remote_plugin_name(Attrs) ->
    handle_remote_plugin_name_attr(proplists:get_value(<<"name">>, Attrs)).

handle_remote_plugin_name_attr(undefined) ->
    exit(remote_plugin_name_required);
handle_remote_plugin_name_attr(Name) -> Name.

install_temp_remote_plugin(Name, Url) ->
    TmpZip = temp_plugin_zip_filename(),
    e2_log:info({get_remote_plugin, Url, TmpZip}),
    handle_get_remote_plugin(
      genapp_curl:run(
        Url,
        TmpZip,
        ?REMOTE_PLUGIN_CURL_RETRIES,
        ?REMOTE_PLUGIN_CURL_TIMEOUT),
      Name, Url, TmpZip).

temp_plugin_zip_filename() ->
    genapp_util:temp_filename(?REMOTE_PLUGIN_TEMP_PREFIX) ++ ".zip".

handle_get_remote_plugin(ok, Name, _Url, TmpZip) ->
    handle_temp_plugin_install(
      install_temp_plugin_from_zip(TmpZip), Name, TmpZip);
handle_get_remote_plugin({error, Err}, Url, _TmpZip, _PluginAttrs) ->
    exit({get_remote_plugin_error, Err, Url}).

install_temp_plugin_from_zip(Zip) ->
    Dir = make_temp_plugin_dir(Zip),
    handle_temp_plugin_unzip(unzip(Zip, Dir), Dir).

make_temp_plugin_dir(Zip) ->
    make_dir(temp_plugin_dir(Zip)).

temp_plugin_dir(Zip) ->
    genapp_util:filename_join(
      filename:dirname(Zip), filename:basename(Zip, ".zip")).

handle_temp_plugin_unzip(ok, Dir) -> {ok, Dir};
handle_temp_plugin_unzip({error, Err}, _Dir) -> {error, Err}.

handle_temp_plugin_install({ok, Dir}, Name, _TmpZip) ->
    #plugin{dir=Dir, name=Name};
handle_temp_plugin_install({error, Err}, Name, TmpZip) ->
    exit({remote_plugin_install_error, Err, Name, TmpZip}).

add_plugin(Plugin, #state{plugins=undefined}=State) ->
    State#state{plugins=[Plugin]};
add_plugin(Plugin, #state{plugins=Plugins}=State) ->
    State#state{plugins=Plugins ++ [Plugin]}.

%%%===================================================================
%%% Create app directory
%%%===================================================================

create_app_directory(#state{app=App}=State) ->
    {StateId, StateDir} = create_new_app_dir(App),
    apply_stages(
      [app_dir_skel, app_metadata, setup_script],
      update_app(App#app{id=StateId, dir=StateDir}, State)).

create_new_app_dir(#app{id=undefined}) ->
    genapp_resource:create_new_app_dir();
create_new_app_dir(#app{id=Id}) ->
    genapp_resource:create_new_app_dir(Id).

create_app_dir_skel(#state{app=App}=State) ->
    set_initial_app_dir_permissions(App),
    GenStateDir = genapp_dir:root(App),
    make_dir(GenStateDir),
    make_subdirs(
      GenStateDir,
      [?GENAPP_SETUP_STATUS_SUBDIR,
       ?GENAPP_CONTROL_SUBDIR,
       ?GENAPP_LOG_SUBDIR]),
    State.

set_initial_app_dir_permissions(#app{dir=Dir}) ->
    ok = file:change_mode(Dir, 8#00700).

write_app_metadata(#state{app=App}=State) ->
    write_metadata(App, app_metadata_file(App)),
    State.

app_metadata_file(App) ->
    genapp_util:filename_join(genapp_dir:root(App), ?GENAPP_METADATA_FILE).

write_metadata(#app{meta=Meta}, File) ->
    ok = file:write_file(File, jiffy:encode(Meta)).

make_subdirs(_Dir, []) -> ok;
make_subdirs(Dir, [SubDir|Rest]) ->
    ok = file:make_dir(genapp_util:filename_join(Dir, SubDir)),
    make_subdirs(Dir, Rest).

write_app_setup_script(#state{app=App}=State) ->
    Src = genapp_util:filename_join(
            [genapp:priv_dir(), "scripts", "plugin_setup_app"]),
    Dest = plugin_app_setup_script(App),
    {ok, _} = file:copy(Src, Dest),
    ok = file:change_mode(Dest, 8#00755),
    State.

plugin_app_setup_script(#app{dir=Dir}) ->
    genapp_util:filename_join(Dir, "setup").

%%%===================================================================
%%% Create app user
%%%===================================================================

create_app_user(#state{app=App}=State) ->
    User = genapp_user:create(App),
    update_app(App#app{user=User}, State).

%%%===================================================================
%%% Reserve ports
%%%===================================================================

reserve_ports(#state{app=App}=State) ->
    reserve_ports(required_ports(App), State).

required_ports(#app{meta=Meta}) ->
    handle_port_count_metadata(
      genapp_metadata:get_value(
        <<"app">>, <<"required_port_count">>, Meta)).

handle_port_count_metadata(error) -> 1;
handle_port_count_metadata({ok, N}) -> N.

reserve_ports(0, #state{app=App}=State) ->
    update_app(App#app{ports=[]}, State);
reserve_ports(N, #state{app=App}=State) when is_integer(N), N > 0 ->
    handle_reserve_ports(genapp_resource:reserve_app_ports(App, N), State).

handle_reserve_ports({ok, Ports}, #state{app=App}=State) ->
    update_app(App#app{ports=lists:sort(Ports)}, State);
handle_reserve_ports({error, Err}, State) ->
    error({port_reservation_error, Err, State}).

%%%===================================================================
%%% Env
%%%===================================================================

write_env(#state{app=App}=State) ->
    write_env(genapp_env_file(App), metadata_env(App), App),
    State.

genapp_env_file(App) ->
    genapp_util:filename_join(
      genapp_dir:subdir(App, ?GENAPP_CONTROL_SUBDIR),
      ?GENAPP_ENV_FILE).

metadata_env(#app{meta=Meta}) ->
    metadata_env_val(genapp_metadata:get_value(<<"app">>, <<"env">>, Meta)).

metadata_env_val(error) -> [];
metadata_env_val({ok, {Env}}) -> Env.

write_env(File, Env, App) ->
    handle_write_file(write_file(File, render_env_file(Env, App)), File).

render_env_file(MetadataEnv, App) ->
    AppEnv = genapp:app_env(App),
    render_env(MetadataEnv, AppEnv, []).

render_env([], _AppEnv, Acc) -> lists:reverse(Acc);
render_env([{Name, Val}|Rest], AppEnv, Acc) when is_binary(Val) ->
    render_env(Rest, AppEnv, [env_assignment(Name, Val, AppEnv)|Acc]);
render_env([_Other|Rest], AppEnv, Acc) ->
    render_env(Rest, AppEnv, Acc).

env_assignment(Name, Val, AppEnv) ->
    [Name, <<"=\"">>, bash_expand(escape_quotes(Val), AppEnv), <<"\"\n">>].

escape_quotes(Val) ->
    re:replace(Val, "\"", "\\\\\"", [global, {return, list}]).

bash_expand(Val, AppEnv) ->
    escape_quotes(bash_echo(Val, AppEnv)).

bash_echo(Val, Env) ->
    {0, Out} =
        genapp_cmd:run(
          "bash", ["-c", ["echo -n \"", Val, "\""]], [{env, Env}]),
    Out.

write_file(File, Bytes) ->
    file:write_file(File, Bytes).

handle_write_file(ok, _File) -> ok;
handle_write_file({error, Err}, File) ->
    error({file_write_error, Err, File}).

%%%===================================================================
%%% Pre app setup
%%%===================================================================

pre_setup(#state{app=App}=State) ->
    Dir = app_dir(App),
    User = app_user(App),
    pre_setup_set_owner(genapp:mode(), Dir, User, User),
    change_mode(Dir, 8#00750),
    State.

pre_setup_set_owner(normal, Dir, User, Group) ->
    set_owner(Dir, User, Group);
pre_setup_set_owner(devmode, _Dir, _User, _Group) ->
    ok.

set_owner(Dir, User, Group) ->
    {0, ""} = genapp_cmd:run("chown", ["-R", User ++ ":" ++ Group, Dir]).

change_mode(Dir, Mode) ->
    ok = file:change_mode(Dir, Mode).

%%%===================================================================
%%% App setup
%%%===================================================================

plugins_setup(#state{plugins=Plugins}=State) ->
    plugins_setup(Plugins, State).

plugins_setup([], State) -> State;
plugins_setup([Plugin|Rest], State) ->
    plugin_setup(Plugin, State),
    plugins_setup(Rest, State).

plugin_setup(#plugin{dir=PluginDir, name=PluginName},
             #state{app=App}=State) ->
    handle_plugin_setup_app(
      genapp_plugin:setup_app(PluginDir, App), PluginName, State).

handle_plugin_setup_app({0, Out}, Plugin, #state{app=App}) ->
    plugin_setup_result(Plugin, 0, Out, App),
    e2_log:info({plugin_setup_ok, {Plugin, app_id(App), Out}});
handle_plugin_setup_app({N, Err}, Plugin, #state{app=App}=State) ->
    plugin_setup_result(Plugin, N, Err, App),
    setup_status(error, State),
    error({plugin_setup, {Plugin, app_id(App), {N, Err}}}).

%%%===================================================================
%%% Post app setup
%%%===================================================================

post_setup(#state{app=App}=State) ->
    post_setup_set_owner(genapp:mode(), App),
    GenAppDir = genapp_dir:root(App),
    set_dir_readonly(GenAppDir),
    set_subdirs_group_writeable(GenAppDir, [?GENAPP_LOG_SUBDIR]),
    delete_app_startup_script(App),
    State.

post_setup_set_owner(normal, #app{dir=Dir, user=User}) ->
    set_owner(Dir, "root", User);
post_setup_set_owner(devmode, _State) ->
    ok.

set_dir_readonly(Dir) ->
    {0, ""} = genapp_cmd:run("chmod", ["-R", "go-w", Dir]).

set_subdirs_group_writeable(_Dir, []) -> ok;
set_subdirs_group_writeable(Dir, [Subdir|Rest]) ->
    Path = genapp_util:filename_join(Dir, Subdir),
    {0, ""} = genapp_cmd:run("chmod", ["-R", "g+w", Path]),
    set_subdirs_group_writeable(Dir, Rest).

delete_app_startup_script(State) ->
    ok = file:delete(plugin_app_setup_script(State)).

%%%===================================================================
%%% Setup status
%%%===================================================================

setup_status(Status, #state{app=App}=State) ->
    delete_all_status(?SETUP_STATUS, App),
    touch_status(Status, App),
    State.

delete_all_status([], _) -> ok;
delete_all_status([Status|Rest], App) ->
    delete_status(Status, App),
    delete_all_status(Rest, App).

delete_status(Status, App) ->
    check_status_delete(file:delete(status_file(Status, App))).

check_status_delete(ok) -> ok;
check_status_delete({error, enoent}) -> ok.

status_file(Status, App) ->
    genapp_util:filename_join(
      genapp_dir:subdir(App, ?GENAPP_SETUP_STATUS_SUBDIR),
      atom_to_list(Status)).

touch_status(Status, App) ->
    ok = file:write_file(status_file(Status, App), <<>>).

plugin_setup_result(Plugin, Exit, Out, App) ->
    ok = file:write_file(plugin_setup_result_file(Plugin, Exit, App), Out).

plugin_setup_result_file(Plugin, Exit, App) ->
    genapp_util:filename_join(
      genapp_dir:subdir(App, ?GENAPP_SETUP_STATUS_SUBDIR),
      plugin_setup_result_name(Plugin, Exit)).

plugin_setup_result_name(Plugin, Exit) ->
    "plugin_" ++ binary_to_list(Plugin) ++ "_" ++ integer_to_list(Exit).

%%%===================================================================
%%% Notification
%%%===================================================================

extension_notify_app_event(Event, #state{app=App}=State) ->
    genapp_extension:notify_app_event(Event, App),
    State.

%%%===================================================================
%%% Misc
%%%===================================================================

update_app(App, State) -> State#state{app=App}.

app_id(#app{id=Id}) -> Id.

app_user(#app{user=User}) -> User.

app_dir(#app{dir=Dir}) -> Dir.

make_dir(Dir) ->
    handle_make_dir(file:make_dir(Dir), Dir).

handle_make_dir(ok, Dir) -> Dir;
handle_make_dir({error, Err}, Dir) ->
    exit({make_dir_error, Err, Dir}).

unzip(Zip, IntoDir) ->
    RelZip = genapp_util:filename_join("..", Zip),
    handle_unzip_result(
      genapp_cmd:run(
        "unzip", ["-qn", RelZip], [{cd, IntoDir}],
        ?UNZIP_TIMEOUT_SECONDS * 1000)).

handle_unzip_result({0, ""}) -> ok;
handle_unzip_result({N, Err}) -> {error, {N, Err}}.
