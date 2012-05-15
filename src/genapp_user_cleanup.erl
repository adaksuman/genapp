-module(genapp_user_cleanup).

-behavior(e2_task).

-export([start_link/0]).

-export([init/1, handle_task/1]).

-record(state, {apps_home}).

-define(INTERVAL, 15000).
-define(ETC_PASSWD, "/etc/passwd").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    e2_task:start_link(?MODULE, []).

%%%===================================================================
%%% Init
%%%===================================================================

init([]) ->
    {ok, init_state(), timings()}.

init_state() ->
    #state{apps_home=genapp:get_env(apps_home)}.

timings() ->
    {0, ?INTERVAL}.

%%%===================================================================
%%% Cleanup task
%%%===================================================================

handle_task(State) ->
    delete_users(orphaned_users(app_users(), State)),
    {repeat, State}.

app_users() ->
    {ok, Bin} = file:read_file(?ETC_PASSWD),
    handle_parse_users(parse_users(Bin)).

handle_parse_users({match, Matches}) -> [User || [User] <- Matches];
handle_parse_users(nomatch) -> [].

parse_users(Bin) ->
    re:run(Bin, "^(app_[^:]+):", [{capture, [1], list}, global, multiline]).

orphaned_users(Users, State) ->
    lists:filter(fun(User) -> app_dir_missing(User, State) end, Users).

app_dir_missing("app_" ++ ResId, #state{apps_home=AppsHome}) ->
    not filelib:is_dir(filename:join(AppsHome, ResId)).

delete_users([]) -> ok;
delete_users(Users) ->
    e2_log:info({genapp_user_cleanup, Users}),
    lists:foreach(fun genapp_user:delete/1, Users).
