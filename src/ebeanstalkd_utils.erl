-module(ebeanstalkd_utils).

-export([lookup/3, to_bin/1, env/1]).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} ->
            Result;
        false ->
            Default
    end.

env(Attr) ->
    case application:get_env(ebeanstalkd, Attr) of
        {ok, Value} ->
            Value;
        _ ->
            undefined
    end.

to_bin(Data) when is_binary(Data) ->
    Data;
to_bin(Data) when is_integer(Data) ->
    integer_to_binary(Data);
to_bin(Data) when is_list(Data) ->
    iolist_to_binary(Data).