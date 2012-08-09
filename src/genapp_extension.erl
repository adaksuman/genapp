-module(genapp_extension).

-behavior(e2_service).

-export([start_link/0,
         register/2,
         run/4,
         run_as/5,
         notify_app_event/2]).

-export([handle_msg/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [].

-record(state, {run, run_as, app_event_listeners=[]}).

start_link() ->
    e2_service:start_link(?MODULE, #state{}, [registered]).

register(Name, Fun) ->
    e2_service:call(?MODULE, {reg, validate(Name, Fun)}).

run(Cmd, Args, Options, Timeout) ->
    call(run, [Cmd, Args, Options, Timeout]).

run_as(User, Cmd, Args, Options, Timeout) ->
    call(run_as, [User, Cmd, Args, Options, Timeout]).

call(Name, Args) ->
    call(lookup(Name), Args, Name).

notify_app_event(Event, App) ->
    call_app_event_listeners(Event, App, (lookup(app_event_listeners))).

call_app_event_listeners(_Event, App, []) -> App;
call_app_event_listeners(Event, App, [Listener|Rest]) ->
    call_app_event_listeners(
      Event, call(Listener, [Event, App], handle_app_event), Rest).

-define(valid_name(N),
        N == run orelse
        N == run_as orelse
        N == app_event_listener).

validate(Name, Fun)
  when ?valid_name(Name), is_function(Fun) ->
    {Name, Fun};
validate(Name, {M, F})
  when ?valid_name(Name), is_atom(M), is_atom(F) ->
    {Name, {M, F, []}};
validate(Name, {M, F, A})
  when ?valid_name(Name), is_atom(M), is_atom(F), is_list(A) ->
    {Name, {M, F, A}};
validate(Name, Fun) -> error({invalid_handler, {Name, Fun}}).

lookup(Name) ->
    e2_service:call(?MODULE, {get, Name}).

call(undefined, _Args, Name) ->
    error({undefined_genapp_extension_impl, Name});
call(Fun, Args, _Name) when is_function(Fun) ->
    erlang:apply(Fun, Args);
call({M, F, []}, Args, _Name) ->
    erlang:apply(M, F, Args);
call({M, F, Base}, Args, _Name) ->
    erlang:apply(M, F, Args ++ Base).

handle_msg({reg, {run, F}}, _From, State) ->
    {reply, ok, State#state{run=F}};
handle_msg({reg, {run_as, F}}, _From, State) ->
    {reply, ok, State#state{run_as=F}};
handle_msg({reg, {app_event_listener, L}}, _From, State) ->
    {reply, ok, add_app_event_listener(L, State)};
handle_msg({get, run}, _From, State) ->
    {reply, State#state.run, State};
handle_msg({get, run_as}, _From, State) ->
    {reply, State#state.run_as, State};
handle_msg({get, app_event_listeners}, _From, State) ->
    {reply, State#state.app_event_listeners, State}.

add_app_event_listener(L, #state{app_event_listeners=Ls}=State) ->
    State#state{app_event_listeners=[L|Ls]}.
