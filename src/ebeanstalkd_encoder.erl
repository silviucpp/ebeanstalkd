-module(ebeanstalkd_encoder).

-export([encode/2, encode/1]).

-define(END_LINE, "\r\n").

encode(Cmd, Data) ->
    <<(encode(Cmd))/binary, (ebeanstalkd_utils:to_bin(Data))/binary, ?END_LINE>>.

encode(Cmd) ->
    cmd_encode(Cmd).

cmd_encode(Cmd) when is_list(Cmd) ->
    cmd_encode(Cmd, <<>>);
cmd_encode(Cmd) when is_binary(Cmd) ->
    <<Cmd/binary, ?END_LINE>>.

cmd_encode([H|T], <<>>) ->
    cmd_encode(T, ebeanstalkd_utils:to_bin(H));
cmd_encode([H|T], Acc) ->
    cmd_encode(T, <<Acc/binary, " ", (ebeanstalkd_utils:to_bin(H))/binary>>);
cmd_encode([], Acc) ->
    <<Acc/binary, ?END_LINE>>.

