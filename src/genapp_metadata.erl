-module(genapp_metadata).

-export([parse/1, parse_file/1, get_value/2, names/1]).

parse(Bin) when is_binary(Bin) ->
    jiffy:decode(Bin).

parse_file(File) ->
    handle_read_file(file:read_file(File), File).

handle_read_file({ok, Bin}, _File) ->
    parse(Bin);
handle_read_file({error, Err}, File) ->
    error({metadata_read, {Err, File}}).

get_value(Name, {Attrs}) when is_list(Attrs) ->
    get_attr(binary(Name), Attrs);
get_value(_Name, _Other) ->
    error.

binary(Str) when is_list(Str) -> list_to_binary(Str);
binary(Bin) when is_binary(Bin) -> Bin.

get_attr(_Name, []) -> error;
get_attr(Name, [{Name, Value}|_]) -> {ok, Value};
get_attr(Name, [_|Rest]) -> get_attr(Name, Rest).

names({Attrs}) when is_list(Attrs) ->
    [Name || {Name, _} <- Attrs];
names(_) -> [].
