-module(ebeanstalkd_decoder).

-include("ebeanstalkd.hrl").

-define(RESPONSE_MAPPING, #{
    <<"OUT_OF_MEMORY">> => out_of_memory,
    <<"INTERNAL_ERROR">> => internal_error,
    <<"DRAINING">> => draining,
    <<"BAD_FORMAT">> => bad_format,
    <<"UNKNOWN_COMMAND">> => unknown_command,
    <<"EXPECTED_CRLF">> => expected_crlf,
    <<"JOB_TOO_BIG">> => job_too_big,
    <<"DEADLINE_SOON">> => deadline_soon,
    <<"TIMED_OUT">> => timed_out,
    <<"DELETED">> => deleted,
    <<"NOT_FOUND">> => not_found,
    <<"RELEASED">> => released,
    <<"BURIED">> => buried,
    <<"TOUCHED">> => touched,
    <<"KICKED">> => kicked,
    <<"NOT_IGNORED">> => not_ignored,
    <<"INSERTED">> => inserted
}).

-export([
    new/0,
    decode/2
]).

-record(state, {
    response_map,
    buffer
}).

new() ->
    #state{
        response_map = ?RESPONSE_MAPPING,
        buffer = <<>>
    }.

decode(Buffer0, #state{response_map = Rm, buffer = ExistingBuffer} = State) ->
    Buffer = append(ExistingBuffer, Buffer0),

    case binary:split(Buffer, ?BIN_END_LINE) of
        [Command, Rest] ->
            case Command of
                <<"INSERTED ", Bin/bytes>> ->
                    decode_integer(inserted, Bin, State#state{buffer = Rest});
                <<"BURIED ", Bin/bytes>> ->
                    decode_integer(buried, Bin, State#state{buffer = Rest});
                <<"WATCHING ", Bin/bytes>> ->
                    decode_integer(watching, Bin, State#state{buffer = Rest});
                <<"KICKED ", Bin/bytes>> ->
                    decode_integer(kicked, Bin, State#state{buffer = Rest});
                <<"USING ", Bin/bytes>> ->
                    {ok, {using, Bin}, State#state{buffer = Rest}};
                <<"RESERVED ", Bin/bytes>> ->
                    case decode_job(Bin, Rest) of
                        {ok, JobId, JobBody, NewRest} ->
                            {ok, {reserved, JobId, JobBody}, State#state{buffer = NewRest}};
                        _ ->
                            {more, State#state{buffer = Buffer}}
                    end;
                <<"FOUND ", Bin/bytes>> ->
                    case decode_job(Bin, Rest) of
                        {ok, JobId, JobBody, NewRest} ->
                            {ok, {found, JobId, JobBody}, State#state{buffer = NewRest}};
                        _ ->
                            {more, State#state{buffer = Buffer}}
                    end;
                <<"OK ", Bin/bytes>> ->
                    case decode_body(Bin, Rest) of
                        {ok, Body, NewRest} ->
                            {ok, yaml_parser(Body), State#state{buffer = NewRest}};
                        _ ->
                            {more, State#state{buffer = Buffer}}
                    end;
                Other ->
                    case maps:find(Other, Rm) of
                        {ok, Value} ->
                            {ok, {Value}, State#state{buffer = Rest}};
                        _ ->
                            throw({unknown_command, Other})
                    end
            end;
        _ ->
            {more, State#state{buffer = Buffer}}
    end.

% internals

append(<<>>, B) ->
    B;
append(A, B) ->
    <<A/binary, B/binary>>.

decode_integer(Command, Bin, NewState) ->
    {ok, {Command, binary_to_integer(Bin)}, NewState}.

decode_job(CommandPayload, Rest) ->
    case binary:split(CommandPayload, ?BIN_WHITE_SPACE) of
        [Id0, JobBytes0] ->
            Id = binary_to_integer(Id0),
            JobBytes = binary_to_integer(JobBytes0),
            case Rest of
                <<Body:JobBytes/binary, ?STR_END_LINE, NewRest/binary>> ->
                    {ok, Id, Body, NewRest};
                _ ->
                    more
            end;
        _ ->
            more
    end.

decode_body(JobBytes0, Rest) ->
    JobBytes = binary_to_integer(JobBytes0),
    case Rest of
        <<Body:JobBytes/binary, ?STR_END_LINE, NewRest/binary>> ->
            {ok, Body, NewRest};
        _ ->
            more
    end.

yaml_parser(YamlBin) ->
    try
        [Yaml] = yamerl_constr:string(YamlBin, [str_node_as_binary]),
        {ok, Yaml}
    catch _:Error ->
        {erorr, Error}
    end.
