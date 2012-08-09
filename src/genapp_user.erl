%%%===================================================================
%%% @doc Manages genapp system users.
%%% @end
%%%===================================================================

-module(genapp_user).

-export([create/1, delete/1, app_user/1]).

-include("genapp.hrl").

-define(USER_NOT_EXISTS, 6).
-define(NO_USER_PROCS, 1).
-define(USER_CMD_TIMEOUT, 30000).

create(App) ->
    create(genapp:mode(), App).

create(normal,  #app{dir=Dir, user=undefined}=App) ->
    User = app_user(App),
    handle_useradd(user_cmd("useradd", ["-d", Dir, User]), User);
create(devmode, #app{user=undefined}) ->
    current_user().

current_user() ->
    handle_user_env(os:getenv("USER")).

handle_user_env(false) -> error(no_user_env);
handle_user_env(User) -> User.

app_user(#app{id=Id}) -> app_user(Id);
app_user(AppId) -> "app_" ++ AppId.

handle_useradd({0, _}, User) -> User;
handle_useradd({_, Err}, _User) -> error(Err).

delete(User) when is_list(User) ->
    delete(#app{user=User});
delete(#app{user=User}) when is_list(User) ->
    kill_procs(User),
    handle_userdel(user_cmd("userdel", ["-f", User])).

handle_userdel({0, _}) -> ok;
handle_userdel({?USER_NOT_EXISTS, _}) -> ok;
handle_userdel({N, Err}) -> error({N, Err}).

kill_procs(User) ->
    handle_pkill(genapp_cmd:run("pkill", ["-9", "-u", User])).

handle_pkill({0, _}) -> ok;
handle_pkill({?NO_USER_PROCS, _}) -> ok;
handle_pkill({N, Err}) -> error({N, Err}).

user_cmd(Exe, Args) ->
    genapp_cmd:run(Exe, Args, [], ?USER_CMD_TIMEOUT).
