-module(genapp_deploy).

-behavior(e2_task).

-include("genapp.hrl").

-export([start_link/1, start_link/2]).

-export([init/1, handle_task/1]).

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
    {ok, new_app(PkgDir, Options)}.

new_app(PkgDir, Options) ->
    Meta = genapp_metadata:parse_file(metadata_file(PkgDir)),
    #app{id=app_id(Options), meta=Meta, pkg_dir=PkgDir}.

metadata_file(PkgDir) ->
    filename:join([PkgDir, ?GENAPP_SUBDIR, ?GENAPP_METADATA_FILE]).

app_id(Options) ->
    proplists:get_value(id, Options).

%%%===================================================================
%%% Deploy task
%%%===================================================================

handle_task(App) ->
    apply_stages(App, [check_plugins, reservation, setup, {setup_status, ok}]),
    {stop, normal}.

apply_stages(App, []) -> App;
apply_stages(App, [Stage|Rest]) ->
    notify(Stage, App),
    handle_stage_result(stage(Stage, App), App, Rest).

handle_stage_result(Substages, App, Rest) when is_list(Substages) ->
    apply_stages(apply_stages(App, Substages), Rest);
handle_stage_result(App, _App0, Rest) ->
    apply_stages(App, Rest).

notify(Stage, App) ->
    genapp_event:publish_app_deploy(self(), {Stage, App}).

%%%===================================================================
%%% Stage dispatch
%%%===================================================================

stage(check_plugins, App)->
    check_plugins(App);
stage(reservation, _) ->
    [app_directory, {setup_status, pending}, app_user, ports];
stage(setup, _) ->
    [pre_setup, plugins_setup, post_setup, {extension_notify, setup}];
stage(app_directory, App) ->
    create_app_directory(App);
stage(app_user, App) ->
    create_app_user(App);
stage(ports, App) ->
    reserve_ports(App);
stage(pre_setup, App) ->
    pre_setup(App);
stage(plugins_setup, App) ->
    plugins_setup(App);
stage(post_setup, App) ->
    post_setup(App);
stage(app_dir_skel, App) ->
    create_app_dir_skel(App);
stage(app_metadata, App) ->
    write_app_metadata(App);
stage(setup_script, App) ->
    write_app_setup_script(App);
stage({setup_status, Status}, App) ->
    setup_status(Status, App);
stage({extension_notify, Event}, App) ->
    extension_notify_app_event(Event, App).

%%%===================================================================
%%% Check plugins
%%%===================================================================

check_plugins(App) ->
    check_plugins_installed(check_plugins_defined(app_plugins(App))),
    App.

app_plugins(#app{meta=Meta}) ->
    handle_plugins_metadata(genapp_metadata:get_value("app.plugins", Meta)).

handle_plugins_metadata(error) -> [];
handle_plugins_metadata({ok, Plugins}) -> Plugins.

check_plugins_defined([]) -> exit(missing_app_plugins);
check_plugins_defined(Plugins) -> Plugins.

check_plugins_installed([]) -> ok;
check_plugins_installed([Plugin|Rest]) ->
    check_plugin_installed(Plugin),
    check_plugins_installed(Rest).

check_plugin_installed(Plugin) ->
    handle_plugin_installed(genapp_plugin:is_installed(Plugin), Plugin).

handle_plugin_installed(true, _Plugin) -> ok;
handle_plugin_installed(false, Plugin) ->
    exit({plugin_not_installed, Plugin}).

%%%===================================================================
%%% Create app directory
%%%===================================================================

create_app_directory(App) ->
    {AppId, AppDir} = create_new_app_dir(App),
    apply_stages(App#app{id=AppId, dir=AppDir},
                 [app_dir_skel, app_metadata, setup_script]).

create_new_app_dir(#app{id=undefined}) ->
    genapp_resource:create_new_app_dir();
create_new_app_dir(#app{id=Id}) ->
    genapp_resource:create_new_app_dir(Id).

create_app_dir_skel(App) ->
    set_initial_app_dir_permissions(App),
    GenAppDir = genapp_dir:root(App),
    make_dir(GenAppDir),
    make_subdirs(GenAppDir, [?GENAPP_SETUP_STATUS_SUBDIR,
			     ?GENAPP_CONTROL_SUBDIR,
			     ?GENAPP_LOG_SUBDIR]),
    App.

set_initial_app_dir_permissions(#app{dir=Dir}) ->
    ok = file:change_mode(Dir, 8#00700).

write_app_metadata(App) ->
    write_metadata(App, app_metadata_file(App)),
    App.

app_metadata_file(App) ->
    filename:join(genapp_dir:root(App), ?GENAPP_METADATA_FILE).

write_metadata(#app{meta=Meta}, File) ->
    ok = file:write_file(File, jiffy:encode(Meta)).

make_dir(Dir) ->
    ok = file:make_dir(Dir).

make_subdirs(_Dir, []) -> ok;
make_subdirs(Dir, [SubDir|Rest]) ->
    ok = file:make_dir(filename:join(Dir, SubDir)),
    make_subdirs(Dir, Rest).

write_app_setup_script(App) ->
    Src = filename:join([genapp:priv_dir(), "scripts", "plugin_setup_app"]),
    Dest = plugin_app_setup_script(App),
    {ok, _} = file:copy(Src, Dest),
    ok = file:change_mode(Dest, 8#00755),
    App.

plugin_app_setup_script(#app{dir=Dir}) ->
    filename:join(Dir, "setup").

%%%===================================================================
%%% Create app user
%%%===================================================================

create_app_user(App) ->
    User = genapp_user:create(App),
    App#app{user=User}.

%%%===================================================================
%%% Reserve ports
%%%===================================================================

reserve_ports(#app{meta=Meta}=App) ->
    handle_required_ports(
      genapp_metadata:get_value("app.required_port_count", Meta), App).

handle_required_ports(error, App) ->
    handle_required_ports({ok, 1}, App);
handle_required_ports({ok, Count}, App) when is_integer(Count), Count > 0 ->
    handle_reserved_ports(genapp_resource:reserve_app_ports(App, Count), App).

handle_reserved_ports({ok, Ports}, App) ->
    App#app{ports=lists:sort(Ports)};
handle_reserved_ports({error, Err}, App) ->
    error({port_reservation_error, Err, App}).

%%%===================================================================
%%% Pre app setup
%%%===================================================================

pre_setup(#app{dir=Dir, user=User}=App) ->
    pre_setup_set_owner(genapp:mode(), Dir, User, User),
    change_mode(Dir, 8#00750),
    App.

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

plugins_setup(App) ->
    plugins_setup(app_plugins(App), App).

plugins_setup([], App) -> App;
plugins_setup([Plugin|Rest], App) ->
    plugin_setup(Plugin, App),
    plugins_setup(Rest, App).

plugin_setup(Plugin, App) ->
    handle_plugin_setup_app(genapp_plugin:setup_app(Plugin, App), Plugin, App).

handle_plugin_setup_app({0, Out}, Plugin, #app{id=Id}=App) ->
    plugin_setup_result(Plugin, 0, Out, App),
    e2_log:info({plugin_setup_ok, {Plugin, Id, Out}});
handle_plugin_setup_app({N, Err}, Plugin, #app{id=Id}=App) ->
    plugin_setup_result(Plugin, N, Err, App),
    setup_status(error, App),
    error({plugin_setup, {Plugin, Id, {N, Err}}}).

%%%===================================================================
%%% Post app setup
%%%===================================================================

post_setup(App) ->
    post_setup_set_owner(genapp:mode(), App),
    GenAppDir = genapp_dir:root(App),
    set_dir_readonly(GenAppDir),
    set_subdirs_group_writeable(GenAppDir, [?GENAPP_LOG_SUBDIR]),
    delete_app_startup_script(App),
    App.

post_setup_set_owner(normal, #app{dir=Dir, user=User}) ->
    set_owner(Dir, "root", User);
post_setup_set_owner(devmode, _App) ->
    ok.

set_dir_readonly(Dir) ->
    {0, ""} = genapp_cmd:run("chmod", ["-R", "go-w", Dir]).

set_subdirs_group_writeable(_Dir, []) -> ok;
set_subdirs_group_writeable(Dir, [Subdir|Rest]) ->
    {0, ""} = genapp_cmd:run(
                "chmod", ["-R", "g+w", filename:join(Dir, Subdir)]),
    set_subdirs_group_writeable(Dir, Rest).

delete_app_startup_script(App) ->
    ok = file:delete(plugin_app_setup_script(App)).

%%%===================================================================
%%% Setup status
%%%===================================================================

setup_status(Status, App) ->
    delete_all_status(?SETUP_STATUS, App),
    touch_status(Status, App),
    App.

delete_all_status([], _) -> ok;
delete_all_status([Status|Rest], App) ->
    delete_status(Status, App),
    delete_all_status(Rest, App).

delete_status(Status, App) ->
    check_status_delete(file:delete(status_file(Status, App))).

check_status_delete(ok) -> ok;
check_status_delete({error, enoent}) -> ok.

status_file(Status, App) ->
    filename:join(
      genapp_dir:subdir(App, ?GENAPP_SETUP_STATUS_SUBDIR),
      atom_to_list(Status)).

touch_status(Status, App) ->
    ok = file:write_file(status_file(Status, App), <<>>).

plugin_setup_result(Plugin, Exit, Out, App) ->
    ok = file:write_file(plugin_setup_result_file(Plugin, Exit, App), Out).

plugin_setup_result_file(Plugin, Exit, App) ->
    filename:join(
      genapp_dir:subdir(App, ?GENAPP_SETUP_STATUS_SUBDIR),
      plugin_setup_result_name(Plugin, Exit)).

plugin_setup_result_name(Plugin, Exit) ->
    "plugin_" ++ binary_to_list(Plugin) ++ "_" ++ integer_to_list(Exit).

%%%===================================================================
%%% Notification
%%%===================================================================

extension_notify_app_event(Event, App) ->
    genapp_extension:notify_app_event(Event, App).
