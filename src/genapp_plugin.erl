%%%===================================================================
%%% @module
%%% Genapp plugin support.
%%%
%%% The functions in this module are thread safe.
%%% @end
%%%===================================================================

-module(genapp_plugin).

-include("genapp.hrl").

-export([local_plugin_dir/1, setup_app/2]).

-define(APP_SETUP_TIMEOUT, 60000).

%%%===================================================================
%%% API
%%%===================================================================

local_plugin_dir(Name) ->
    PluginDir = plugin_dir(Name),
    handle_is_plugin_dir(filelib:is_dir(PluginDir), PluginDir).

handle_is_plugin_dir(true, Dir) -> {ok, Dir};
handle_is_plugin_dir(false, _Dir) -> error.

setup_app(Plugin, App) ->
    setup_app(genapp:mode(), Plugin, App).

setup_app(Mode, PluginDir, #app{dir=AppDir, user=User}=App) ->
    SetupScript = app_setup_script(App),
    SetupEnv = app_setup_env(PluginDir, App),
    RunOptions = [{cd, AppDir}, {env, SetupEnv}],
    run_script(Mode, SetupScript, User, RunOptions, ?APP_SETUP_TIMEOUT).

run_script(normal, Script, User, Options, Timeout) ->
    genapp_extension:run_as(User, Script, [], Options, Timeout);
run_script(devmode, Script, _User, Options, Timeout) ->
    genapp_extension:run(Script, [], Options, Timeout).

%%%===================================================================
%%% Plugin info
%%%===================================================================

plugin_dir(Name) ->
    genapp_util:filename_join(plugins_home(), Name).

plugins_home() ->
    genapp:get_env(plugins_home, ?DEFAULT_PLUGINS_HOME).

app_setup_script(#app{dir=Dir}) ->
    %% The "setup" script must exist in Dir as a pre-req for this call.
    genapp_util:filename_join(Dir, "setup").

%%%===================================================================
%%% Plugin setup env
%%%===================================================================

app_setup_env(PluginDir, #app{id=Id,
                              dir=Dir,
                              user=User,
                              ports=Ports,
                              meta=Meta,
                              pkg_dir=PkgDir}=App) ->
    lists:concat(
      [[{"plugin_dir", PluginDir},
        {"app_id", env_val(Id)},
        {"app_dir", env_val(Dir)},
        {"app_user", env_val(User)},
        {"pkg_dir", env_val(PkgDir)},
        {"genapp_dir", genapp_dir:root(App)},
        {"control_dir", genapp_dir:subdir(App, ?GENAPP_CONTROL_SUBDIR)},
        {"log_dir", genapp_dir:subdir(App, ?GENAPP_LOG_SUBDIR)}],
       app_ports_env(Ports),
       metadata_env(Meta),
       extra_metadata_env(PkgDir)]).

env_val(undefined) -> "";
env_val(Val) -> Val.

%%%===================================================================
%%% Ports env
%%%===================================================================

app_ports_env(undefined) ->
    [{"app_port_count", "0"}];
app_ports_env([]) ->
    [{"app_port_count", "0"}];
app_ports_env([P0|_]=Ports) ->
    acc_ports(
      Ports, 0,
      [{"app_port", integer_to_list(P0)},
       {"app_port_count", integer_to_list(length(Ports))}]).

acc_ports([], _N, Acc) -> Acc;
acc_ports([P|Rest], N, Acc) ->
    acc_ports(
      Rest, N + 1,
      [{"app_port" ++ integer_to_list(N), integer_to_list(P)}|Acc]).

%%%===================================================================
%%% Metadata env
%%%===================================================================

metadata_env(Meta) ->
    [{metadata_env_name(Section, Name), metadata_env_value(Value)}
     || {Section, Name, Value} <- flattened_metadata(Meta)].

flattened_metadata(Meta) ->
    genapp_metadata:flatten(Meta).

metadata_env_name(Section, Name) ->
    iolist_to_binary([safe_env_name(Section), <<"_">>, safe_env_name(Name)]).

safe_env_name(Name) ->
    to_lower(iolist_to_binary(re:replace(Name, "\\W", "_", [global]))).

to_lower(B) when is_binary(B) ->
    list_to_binary(string:to_lower(binary_to_list(B))).

metadata_env_value(B) when is_binary(B) -> B;
metadata_env_value(I) when is_integer(I) -> integer_to_list(I);
metadata_env_value(null) -> "";
metadata_env_value(Obj) -> jiffy:encode(Obj).

%%%===================================================================
%%% Extra metadata env
%%%===================================================================

extra_metadata_env(PkgDir) ->
    metadata_env(read_extra_metadata(PkgDir)).

read_extra_metadata(PkgDir) ->
    ExtraFile = extra_metadata_file(PkgDir),
    maybe_read_metadata(filelib:is_file(ExtraFile), ExtraFile).

extra_metadata_file(PkgDir) ->
    genapp_util:filename_join(
      [PkgDir, ?GENAPP_SUBDIR, ?GENAPP_METADATA_EXTRA_FILE]).

maybe_read_metadata(true, File) ->
    genapp_metadata:parse_file(File);
maybe_read_metadata(false, _File) ->
    genapp_metadata:parse(<<"{}">>).
