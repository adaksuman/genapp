%%%===================================================================
%%% @doc Manges genapp runit services.
%%% @end
%%%===================================================================

-module(genapp_service).

-include("genapp.hrl").

-export([create/1, delete/1]).

create(App) ->
    validate_app(App),
    Home = service_home(App),
    create_service_skel(Home),
    write_run_script(Home, App),
    write_log_script(Home, App),
    Home.

validate_app(App) ->
    check_required_file(app_start_script(App)),
    check_required_dir(app_dir(App)).

service_home(#app{id=Id}) ->
    service_home(Id);
service_home(Id) ->
    filename:join(services_home(), Id).

services_home() ->
    genapp:get_env(services_home, ?DEFAULT_SERVICES_HOME).

app_start_script(#app{dir=Dir}) ->
    filename:join([Dir, ".genapp", "control", "start"]).

app_dir(#app{dir=Dir}) -> Dir.

check_required_file(File) ->
    handle_is_required(filelib:is_file(File), File).

check_required_dir(Dir) ->
    handle_is_required(filelib:is_dir(Dir), Dir).

handle_is_required(true, _What) -> ok;
handle_is_required(false, What) -> error({required, What}).

create_service_skel(Dir) ->
    ok = file:make_dir(Dir),
    ok = file:make_dir(filename:join(Dir, "log")).

write_run_script(ServiceDir, #app{dir=AppDir, user=User}=App)
  when User /= undefined ->
    Vars =
        [{app_dir, AppDir},
         {control_dir, app_control_dir(App)},
         {app_user, User},
         {app_start, app_start_script(App)}],
    make_exe(write_template(template("run"), Vars, run_script(ServiceDir))).

app_control_dir(#app{dir=Dir}) ->
    filename:join([Dir, ".genapp", "control"]).

write_log_script(Dir, #app{user=User}=App) ->
    Vars = [{app_user, User}, {app_dir, app_dir(App)}],
    make_exe(write_template(template("log"), Vars, log_script(Dir))).

log_script(Dir) ->
    filename:join([Dir, "log", "run"]).

template(Name) ->
    filename:join([genapp:priv_dir(), "scripts", "runsv_" ++ Name ++ ".in"]).

run_script(Dir) ->
    filename:join(Dir, "run").

write_template(Template, Vars, Dest) ->
    {ok, In} = file:read_file(Template),
    file:write_file(Dest, render_template(In, Vars)),
    Dest.

render_template(Out, []) -> Out;
render_template(In, [{Name, Val}|Rest]) ->
    render_template(apply_replacement(In, Name, Val), Rest).

apply_replacement(Bin, Name, Val) ->
    re:replace(Bin, name_pattern(Name), iolist_to_binary(Val), [global]).

name_pattern(Name) when is_atom(Name) ->
    NameBin = list_to_binary(atom_to_list(Name)),
    <<"{{ *", NameBin/binary, " *}}">>.

make_exe(File) ->
    ok = file:change_mode(File, 8#00755).

delete(Service) ->
    ServiceHome = service_home(Service),
    maybe_kill_service(ServiceHome),
    delete_service(ServiceHome).

maybe_kill_service(ServiceHome) ->
    Control = filename:join([ServiceHome, "supervise", "control"]),
    maybe_send_kill(file:read_file_info(Control), Control).

maybe_send_kill({ok, _}, Control) ->
    Cmd = "printf x > " ++ Control,
    handle_kill_result(genapp_cmd:run("sh", ["-c", Cmd], [], 1000));
maybe_send_kill({error, enoent}, _Control) -> ok.

handle_kill_result({0, ""}) ->
    timer:sleep(1000);
handle_kill_result({error, timeout}) ->
    ok.

delete_service(ServiceHome) ->
    check_service_home(ServiceHome),
    "" = os:cmd("rm -rf \"" ++ ServiceHome ++ "\"").

check_service_home(Dir) ->
    check_service_home_parts(filename:split(Dir), Dir).

check_service_home_parts(N, Dir) when N < 3 ->
    error({invalid_service_home, Dir});
check_service_home_parts(_N, _Dir) -> ok.
