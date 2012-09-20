-module(genapp_metadata).

-export([parse/1,
         parse_file/1,
         get_value/3,
         sections/1,
         section_names/2,
	 section_values/2,
         flatten/1]).

parse(Bin) when is_binary(Bin) ->
    try
        jiffy:decode(Bin)
    catch
        throw:{error, Err} -> error({json_error, Err})
    end.

parse_file(File) ->
    handle_read_file(file:read_file(File), File).

handle_read_file({ok, Bin}, _File) ->
    parse(Bin);
handle_read_file({error, Err}, File) ->
    error({metadata_read, {Err, File}}).

sections({Attrs}) ->
    proplists:get_keys(Attrs);
sections(_Other) ->
    [].

section_names(Section, {Attrs}) ->
    handle_section_names(proplists:get_value(Section, Attrs)).

handle_section_names({Attrs}) ->
    proplists:get_keys(Attrs);
handle_section_names(_Other) ->
    [].

section_values(Section, {Attrs}) ->
    handle_section_values(proplists:get_value(Section, Attrs)).

handle_section_values({Attrs}) ->
    Attrs;
handle_section_values(_Other) ->
    [].

get_value(Section, Name, {Attrs}) ->
    handle_section_attr(Name, proplists:get_value(Section, Attrs)).

handle_section_attr(Name, {Attrs}) ->
    get_attr(Name, Attrs);
handle_section_attr(_Name, _Other) ->
    error.

get_attr(_Name, []) -> error;
get_attr(Name, [{Name, Value}|_]) -> {ok, Value};
get_attr(Name, [_|Rest]) -> get_attr(Name, Rest).

flatten({Attrs}) ->
    acc_flattened(Attrs, []);
flatten(_Other) ->
    [].

acc_flattened([], Acc) -> lists:reverse(Acc);
acc_flattened([{Section, {Attrs}}|Rest], Acc) ->
    acc_flattened(Rest, acc_flattened(Section, Attrs, Acc));
acc_flattened([Attr|Rest], Acc) ->
    acc_flattened(Rest, [Attr|Acc]).

acc_flattened(_Section, [], Acc) -> Acc;
acc_flattened(Section, [{Name, Value}|Rest], Acc) ->
    acc_flattened(Section, Rest, [{Section, Name, Value}|Acc]).
