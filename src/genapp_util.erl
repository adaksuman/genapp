-module(genapp_util).

-export([bin_to_hex/1]).

bin_to_hex(B) when is_binary(B) ->
    lists:flatten(lists:map(fun int_to_hex/1, binary_to_list(B))).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10, N < 16 -> $a + (N - 10).
