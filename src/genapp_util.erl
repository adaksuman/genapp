-module(genapp_util).

-export([bin_to_hex/1, temp_filename/1, filename_join/2, filename_join/1]).

-include("genapp.hrl").

bin_to_hex(B) when is_binary(B) ->
    lists:flatten(lists:map(fun int_to_hex/1, binary_to_list(B))).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10, N < 16 -> $a + (N - 10).

temp_filename(Prefix) ->
    filename:join(
      genapp:get_env(tmp_dir, ?DEFAULT_TEMP_DIR),
      prefixed_timestamp(Prefix)).

prefixed_timestamp(Prefix) ->
    Prefix ++ integer_to_list(timestamp()).

timestamp() ->
    {M, S, U} = erlang:now(),
    M * 1000000000000 + S * 1000000 + U.

%%-------------------------------------------------------------------
%% filename_join/2 and filename_join/1 are implemented here as to
%% support versions of Erlang prior to 5.8.3, which require list args
%% for the join functions.
%%-------------------------------------------------------------------

filename_join(P1, P2) ->
    filename:join(to_list(P1), to_list(P2)).

filename_join(Ps) ->
    filename:join([to_list(P) || P <- Ps]).

to_list(S) when is_list(S) -> S;
to_list(B) when is_binary(B) -> binary_to_list(B).
