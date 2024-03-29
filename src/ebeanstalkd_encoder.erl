-module(ebeanstalkd_encoder).

-include("ebeanstalkd.hrl").

-export([
    encode/1
]).

encode({Cmd, Data}) ->
    <<(cmd_encode(Cmd))/binary, (ebeanstalkd_utils:to_bin(Data))/binary, ?STR_END_LINE>>;
encode(Cmd) ->
    cmd_encode(Cmd).

% internals

cmd_encode(Cmd) when is_list(Cmd) ->
    cmd_encode(Cmd, []);
cmd_encode(Cmd) when is_binary(Cmd) ->
    <<Cmd/binary, ?STR_END_LINE>>.

cmd_encode([H|T], Acc) ->
    cmd_encode(T, [ebeanstalkd_utils:to_bin(H)|Acc]);
cmd_encode([], Acc0) ->
    <<(ebeanstalkd_utils:join(lists:reverse(Acc0), ?BIN_WHITE_SPACE))/binary, ?STR_END_LINE>>.
