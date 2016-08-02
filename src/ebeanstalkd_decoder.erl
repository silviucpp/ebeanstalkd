-module(ebeanstalkd_decoder).

-define(NEW_LINE, <<"\r\n">>).
-define(WHITE_SPACE, <<" ">>).

-export([decode/1]).

decode(<<"OUT_OF_MEMORY\r\n", Rest/binary>>) ->
    {ok, {out_of_memory}, Rest};
decode(<<"INTERNAL_ERROR\r\n", Rest/binary>>) ->
    {ok, {internal_error}, Rest};
decode(<<"DRAINING\r\n", Rest/binary>>) ->
    {ok, {draining}, Rest};
decode(<<"BAD_FORMAT\r\n", Rest/binary>>) ->
    {ok, {bad_format}, Rest};
decode(<<"UNKNOWN_COMMAND\r\n", Rest/binary>>) ->
    {ok, {unknown_command}, Rest};
decode(<<"EXPECTED_CRLF\r\n", Rest/binary>>) ->
    {ok, {expected_crlf}, Rest};
decode(<<"JOB_TOO_BIG\r\n", Rest/binary>>) ->
    {ok, {job_too_big}, Rest};
decode(<<"DEADLINE_SOON\r\n", Rest/binary>>) ->
    {ok, {deadline_soon}, Rest};
decode(<<"TIMED_OUT\r\n", Rest/binary>>) ->
    {ok, {timed_out}, Rest};
decode(<<"DELETED\r\n", Rest/binary>>) ->
    {ok, {deleted}, Rest};
decode(<<"NOT_FOUND\r\n", Rest/binary>>) ->
    {ok, {not_found}, Rest};
decode(<<"RELEASED\r\n", Rest/binary>>) ->
    {ok, {released}, Rest};
decode(<<"BURIED\r\n", Rest/binary>>) ->
    {ok, {buried}, Rest};
decode(<<"TOUCHED\r\n", Rest/binary>>) ->
    {ok, {touched}, Rest};
decode(<<"KICKED\r\n", Rest/binary>>) ->
    {ok, {kicked}, Rest};
decode(<<"NOT_IGNORED\r\n", Rest/binary>>) ->
    {ok, {not_ignored}, Rest};
decode(<<"INSERTED ", Bin/bytes>>) ->
    parse_int(Bin, inserted);
decode(<<"BURIED ", Bin/bytes>>) ->
    parse_int(Bin, buried);
decode(<<"WATCHING ", Bin/bytes>>) ->
    parse_int(Bin, watching);
decode(<<"KICKED ", Bin/bytes>>) ->
    parse_int(Bin, kicked);
decode(<<"USING ", Bin/bytes>>) ->
    parse_string(Bin, using);
decode(<<"RESERVED ", Bin/bytes>>) ->
    parse_job(Bin, reserved);
decode(<<"FOUND ", Bin/bytes>>) ->
    parse_job(Bin, found);
decode(<<"OK ", Bin/bytes>>) ->
    case parse_body(Bin) of
        {ok, Body, Rest} ->
            {ok, ebeanstalkd_yaml:parse(Body), Rest};
        more ->
            more
    end;
decode(_) ->
    more.

parse_job(Bin, Name) ->
    case parse_int(Bin, Name, ?WHITE_SPACE) of
        {ok, {Name, ID}, Bin2} ->
            case parse_body(Bin2) of
                {ok, Body, Rest} ->
                    {ok, {Name, ID, Body}, Rest};
                more ->
                    more
            end;
        more ->
            more
    end.

parse_body(Bin) ->
    case find_next_token_length(Bin, ?NEW_LINE) of
        {ok, Length, BodyRest} ->
            BodyLength = binary_to_integer(binary:part(Bin, {0, Length})),
            case BodyRest of
                <<Body:BodyLength/binary, "\r\n", Rest/binary>> ->
                    {ok, Body, Rest};
                _ ->
                    more
            end;
        Other ->
            Other
    end.

parse_string(Bin, Name) ->
    case find_next_token_length(Bin, ?NEW_LINE) of
        {ok, Length, Rest} ->
            {ok, {Name, binary:part(Bin, {0, Length})}, Rest};
        Other ->
             Other
    end.

parse_int(Bin, Name) ->
    parse_int(Bin, Name, ?NEW_LINE).

parse_int(Bin, Name, Delimiter) ->
    case find_next_token_length(Bin, Delimiter) of
        {ok, Length, Rest} ->
            {ok, {Name, binary_to_integer(binary:part(Bin, {0, Length}))}, Rest};
        Other ->
            Other
    end.

find_next_token_length(Bin, Delimiter) ->
    find_next_token_length(Delimiter, byte_size(Delimiter), Bin, 0).

find_next_token_length(Delimiter, DelimiterLength, Bin, Acc) ->
    case Bin of
        <<Delimiter:DelimiterLength/binary, Rest/binary>> ->
            {ok, Acc, Rest};
        <<_C, Rest/bytes>> ->
            find_next_token_length(Delimiter, DelimiterLength, Rest, Acc+1);
        <<>> ->
            more
    end.