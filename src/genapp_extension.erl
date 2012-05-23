-module(genapp_extension).

-behavior(e2_service).

-export([start_link/0,
         register/2,
         run/4,
         run_as/5,
         create_service/1]).

-export([handle_msg/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [].

-record(state, {run, run_as, create_service}).

start_link() ->
    e2_service:start_link(?MODULE, #state{}, [registered]).

register(Name, Fun) ->
    e2_service:call(?MODULE, {reg, validate(Name, Fun)}).

run(Cmd, Args, Options, Timeout) ->
    call(run, [Cmd, Args, Options, Timeout]).

run_as(User, Cmd, Args, Options, Timeout) ->
    call(run_as, [User, Cmd, Args, Options, Timeout]).

create_service(App) ->
    call(create_service, [App]).

-define(valid_name(N),
        N == run orelse
        N == run_as orelse
        N == create_service).

validate(Name, Fun)
  when ?valid_name(Name), is_function(Fun) ->
    {Name, Fun};
validate(Name, {M, F})
  when ?valid_name(Name), is_atom(M), is_atom(F) ->
    {Name, {M, F, []}};
validate(Name, {M, F, A})
  when ?valid_name(Name), is_atom(M), is_atom(F), is_list(A) ->
    {Name, {M, F, A}};
validate(_, _) -> error(badarg).

call(Name, Args) ->
    call(lookup(Name), Name, Args).

lookup(Name) ->
    e2_service:call(?MODULE, {get, Name}).

call(undefined, Name, _Args) ->
    error({undefined_genapp_extension_impl, Name});
call(Fun, _Name, Args) when is_function(Fun) ->
    erlang:apply(Fun, Args);
call({M, F, []}, _Name, Args) ->
    erlang:apply(M, F, Args);
call({M, F, Base}, _Name, Args) ->
    erlang:apply(M, F, Args ++ Base).

handle_msg({reg, {run, F}}, _From, State) ->
    {reply, ok, State#state{run=F}};
handle_msg({reg, {run_as, F}}, _From, State) ->
    {reply, ok, State#state{run_as=F}};
handle_msg({reg, {create_service, F}}, _From, State) ->
    {reply, ok, State#state{create_service=F}};
handle_msg({get, run}, _From, State) ->
    {reply, State#state.run, State};
handle_msg({get, run_as}, _From, State) ->
    {reply, State#state.run_as, State};
handle_msg({get, create_service}, _From, State) ->
    {reply, State#state.create_service, State}.
