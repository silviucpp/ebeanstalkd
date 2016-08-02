-module(ebeanstalkd_yaml).

-export([parse/1]).

parse(<<"---\n", Data/bytes>>) ->
    case Data of
        <<"- ", _/bytes>> ->
            {ok, parse_list(Data, [])};
        _ ->
            {ok, parse_key_value(Data, [])}
    end.

parse_list(<<>>, Sequence) ->
    lists:reverse(Sequence);
parse_list(<<"- ", Data/bytes>>, Sequence) ->
    {Value, MoreData} = binary_break_at($\n, Data),
    parse_list(MoreData, [Value | Sequence]).

parse_key_value(<<>>, Mapping) ->
    lists:reverse(Mapping);
parse_key_value(Data, Mapping) ->
    {K, <<" ", Rest/bytes>>} = binary_break_at($:, Data),
    {V, MoreData} = binary_break_at($\n, Rest),
    parse_key_value(MoreData, [{K, V} | Mapping]).

binary_break_at(C, Data) when is_binary(Data) ->
    FullSize = byte_size(Data),
    Size = find_end(C, Data, 0),

    case Size =:= FullSize of
        true ->
            {Data, <<>>};
        _ ->
            <<Value:Size/binary, C, Rest/binary>> = Data,
            {Value, Rest}
    end.

find_end(C, <<H, T/bytes>>, Size) ->
    case H of
        C ->
            Size;
        _ ->
            find_end(C, T, Size + 1)
    end;
find_end(_C, <<>>, Size) ->
    Size.