-module(ebeanstalkd_utils).

-export([
    lookup/3,
    to_bin/1,
    env/1,
    safe_call/2,
    safe_call/3
]).

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

safe_call(Receiver, Message) ->
    safe_call(Receiver, Message, 5000).

safe_call(Receiver, Message, Timeout) ->
    try
        gen_server:call(Receiver, Message, Timeout)
    catch
        exit:{noproc, _} ->
            {error, not_started};
        _: Exception ->
            {error, Exception}
    end.
