%%%===================================================================
%%% @module
%%% Genapp plugin support.
%%%
%%% The functions in this module are thread safe.
%%% @end
%%%===================================================================

-module(genapp_plugin).

-include("genapp.hrl").

-export([is_installed/1, setup_app/2]).

-define(APP_SETUP_TIMEOUT, 60000).

%%%===================================================================
%%% API
%%%===================================================================

is_installed(Name) ->
    filelib:is_file(plugin_conf(Name)).

setup_app(Plugin, App) ->
    setup_app(genapp:mode(), Plugin, App).

setup_app(normal, Plugin, #app{dir=Dir, user=User}=App) ->
    genapp_extension:run_as(
      User, setup_app_script(App), [],
      [{cd, Dir}, {env, app_setup_env(Plugin, App)}],
      ?APP_SETUP_TIMEOUT);
setup_app(devmode, Plugin, #app{dir=Dir}=App) ->
    genapp_extension:run(
      setup_app_script(App), [],
      [{cd, Dir}, {env, app_setup_env(Plugin, App)}],
      ?APP_SETUP_TIMEOUT).

%%%===================================================================
%%% Plugin info
%%%===================================================================

plugin_conf(Name) ->
    filename:join(conf_home(), Name).

conf_home() ->
    genapp:get_env(plugins_conf_home, ?DEFAULT_PLUGINS_CONF_HOME).

setup_app_script(#app{dir=Dir}) ->
    %% The "setup" script must exist in Dir as a pre-req for this call.
    filename:join(Dir, "setup").

app_setup_env(
  Plugin, #app{id=Id,
               dir=Dir,
               user=User,
               ports=Ports,
               meta=Meta,
               meta_home=MetaHome}=App) ->
    lists:concat(
      [[{"plugin_conf", plugin_conf(Plugin)},
        {"app_id", env_val(Id)},
        {"app_dir", env_val(Dir)},
        {"app_user", env_val(User)},
        {"meta_home", env_val(MetaHome)},
        {"genapp_dir", genapp_dir:root(App)},
        {"control_dir", genapp_dir:subdir(App, ?GENAPP_CONTROL_SUBDIR)},
        {"log_dir", genapp_dir:subdir(App, ?GENAPP_LOG_SUBDIR)}],
       app_ports_env(Ports),
       plugin_metadata(Meta)]).

env_val(undefined) -> "";
env_val(Val) -> Val.

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

plugin_metadata(Meta) ->
    acc_plugin_metadata(genapp_metadata:names(Meta), Meta, []).

acc_plugin_metadata([], _Meta, Acc) -> Acc;
acc_plugin_metadata([<<"plugin.", Plugin/binary>>=Name|Rest], Meta, Acc) ->
    {ok, PluginEnv} = genapp_metadata:get_value(Name, Meta),
    PluginNames = genapp_metadata:names(PluginEnv),
    acc_plugin_metadata(
      Rest, Meta, acc_plugin_env(Plugin, PluginNames, PluginEnv, Acc));
acc_plugin_metadata([_|Rest], Meta, Acc) ->
    acc_plugin_metadata(Rest, Meta, Acc).

acc_plugin_env(_Plugin, [], _Env, Acc) -> Acc;
acc_plugin_env(Plugin, [Name|Rest], Env, Acc) ->
    {ok, Val} = genapp_metadata:get_value(Name, Env),
    NameVal = {plugin_env_name(Plugin, Name), plugin_env_val(Val)},
    acc_plugin_env(Plugin, Rest, Env, [NameVal|Acc]).

plugin_env_name(Plugin, Name) ->
    SafeName = safe_env_name(Name),
    <<"plugin_", Plugin/binary, "_", SafeName/binary>>.

safe_env_name(Name) ->
    to_lower(iolist_to_binary(re:replace(Name, "\\W", "_", [global]))).

to_lower(B) when is_binary(B) ->
    list_to_binary(string:to_lower(binary_to_list(B))).

plugin_env_val(B) when is_binary(B) -> B;
plugin_env_val(I) when is_integer(I) -> integer_to_list(I);
plugin_env_val(null) -> "".
