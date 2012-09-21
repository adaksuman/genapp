-module(genapp_dir).

-include("genapp.hrl").

-export([root/1, subdir/2]).

root(#app{dir=Dir}) ->
    genapp_util:filename_join(Dir, ?GENAPP_SUBDIR);
root(AppId) ->
    AppsHome = genapp:get_env(apps_home, ?DEFAULT_APPS_HOME),
    genapp_util:filename_join([AppsHome, AppId, ?GENAPP_SUBDIR]).

subdir(App, SubDir) ->
    genapp_util:filename_join(root(App), SubDir).
