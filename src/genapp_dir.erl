-module(genapp_dir).

-include("genapp.hrl").

-export([root/1, subdir/2]).

root(#app{dir=Dir}) ->
    filename:join(Dir, ?GENAPP_SUBDIR);
root(AppId) ->
    AppsHome = genapp:get_env(apps_home, ?DEFAULT_APPS_HOME),
    filename:join([AppsHome, AppId, ?GENAPP_SUBDIR]).

subdir(App, SubDir) ->
    filename:join(root(App), SubDir).
