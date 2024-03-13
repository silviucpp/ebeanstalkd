-module(ebeanstalkd_connection).

-include("ebeanstalkd.hrl").

-export([
    start_link/1,
    stop/1,
    init/2,

    % system callbacks: https://www.erlang.org/doc/design_principles/spec_proc.html

    system_continue/3,
    system_terminate/4,
    system_code_change/4
]).

-record(state, {
    socket,
    host,
    port,
    timeout,
    recon_interval,
    tube,
    monitor,
    queue,
    queue_length,
    decoder_state
}).

-define(CONNECT_OPTIONS,  [
    {mode, binary},
    {packet, raw},
    {keepalive, true},
    {nodelay, true},
    {delay_send, false},
    {send_timeout, 10000},
    {send_timeout_close, true},
    {active, true}
]).

start_link(Options) ->
    proc_lib:start_link(?MODULE, init, [self(), Options]).

stop(Pid) ->
    Pid ! stop,
    ok.

init(Parent, Options) ->

    proc_lib:init_ack(Parent, {ok, self()}),

    Host = ebeanstalkd_utils:lookup(host, Options, ?DEFAULT_IP),
    Port = ebeanstalkd_utils:lookup(port, Options, ?DEFAULT_PORT),
    Timeout = ebeanstalkd_utils:lookup(timeout, Options, ?DEFAULT_CONNECTION_TIMEOUT_MS),
    Tube = ebeanstalkd_utils:lookup(tube, Options, ?DEFAULT_TUBE),
    RecInterval = ebeanstalkd_utils:lookup(reconnect_interval, Options, ?RECONNECT_MS),
    Monitor = ebeanstalkd_utils:lookup(monitor, Options, undefined),

    NewState = connect(#state{
        host = Host,
        port = Port,
        tube = Tube,
        timeout = Timeout,
        recon_interval = RecInterval,
        monitor = Monitor,
        queue = queue:new(),
        queue_length = 0,
        decoder_state = ebeanstalkd_decoder:new()
    }),

    case connection_loop(Parent, NewState) of
        {crash, Class, Error, Stacktrace} ->
            erlang:raise(Class, Error, Stacktrace);
        Other ->
            exit(Other)
    end.

connection_loop(Parent, State) ->
    try
        receive
            {command, From, Tag, CommandPayload} ->
                connection_loop(Parent, send_command(From, Tag, State, CommandPayload));
            {batch_command, From, Tag, Commands} ->
                connection_loop(Parent, send_batch(From, Tag, State, Commands));
            {tcp, _Socket, Packet} ->
                NewState = case decode_packet(Packet, State) of
                    {ok, NewState0} ->
                        NewState0;
                    {error, ErrorMsg, NewState0} ->
                        ?LOG_WARNING("failed to parse protocol: ~p", [ErrorMsg]),
                        disconnect(NewState0)
                end,
                connection_loop(Parent, NewState);
            {tcp_closed, _Socket} ->
                connection_loop(Parent, disconnect(State));
            reconnect ->
                connection_loop(Parent, reconnect(State));
            stop ->
                terminate(normal, State);
            {system, From, Request} ->
                sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
            UnexpectedMessage ->
                ?LOG_WARNING("received unexpected message: ~p", [UnexpectedMessage]),
                connection_loop(Parent, State)
        end
    catch
        ?EXCEPTION(Class, Error, Stacktrace) ->
            ?LOG_WARNING("exception received: ~p stack: ~p", [Error, ?GET_STACK(Stacktrace)]),
            terminate({crash, Class, Error, ?GET_STACK(Stacktrace)}, State)
    end.

system_continue(Parent, _Deb, State) ->
    connection_loop(Parent, State).

system_terminate(Reason, _Parent, _Deb, State) ->
    terminate(Reason, State).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.

send_command(FromPid, Tag, #state{queue = Queue, queue_length = QueueLength, socket = Socket} = State, Command) ->
    case Socket of
        undefined ->
            reply(FromPid, Tag, {error, not_connected}),
            State;
        _ ->
            case QueueLength > ?MAX_PENDING_REQUESTS_QUEUE of
                true ->
                    reply(FromPid, Tag, {error, full_queue}),
                    State;
                _ ->
                    case gen_tcp:send(Socket, ebeanstalkd_encoder:encode(Command)) of
                        ok ->
                            State#state{queue = queue:in({FromPid, Tag}, Queue), queue_length = QueueLength + 1};
                        UnexpectedResult ->
                            reply(FromPid, Tag, UnexpectedResult),
                            disconnect(State)
                    end
            end
    end.

send_batch(FromPid, Tag, #state{queue = Queue, queue_length = QueueLength, socket = Socket} = State, Commands) ->

    case Socket of
        undefined ->
            reply(FromPid, Tag, {error, not_connected}),
            State;
        _ ->
            case QueueLength > ?MAX_PENDING_REQUESTS_QUEUE of
                true ->
                    reply(FromPid, Tag, {error, full_queue}),
                    State;
                _ ->
                    EncodedPayload = lists:map(fun(C) -> ebeanstalkd_encoder:encode(C) end, Commands),

                    case gen_tcp:send(Socket, EncodedPayload) of
                        ok ->
                            Length = length(Commands),
                            Queue1 = lists:foldl(fun(_, Q) -> queue:in({undefined, undefined}, Q) end, Queue, lists:seq(1, Length-1)),
                            State#state{queue = queue:in({FromPid, Tag}, Queue1), queue_length = QueueLength + Length};
                        Error ->
                            reply(FromPid, Tag, Error),
                            disconnect(State)
                    end
            end
    end.

reconnect(#state{host = Host, port = Port} = State) ->
    ?LOG_INFO("try to reconnect to ip: ~p port: ~p", [Host, Port]),
    connect(State).

terminate(Reason, #state{socket = Socket, queue = Queue}) ->
    case Reason of
        normal ->
            clear_queue(Queue, {error, connection_closed});
        {crash, Class, Error, Stack} ->
            clear_queue(Queue, {Class, {Error, Stack}});
        _ ->
            clear_queue(Queue, {error, Reason})
    end,

    case Socket of
        undefined ->
            ok;
        _ ->
            catch gen_tcp:close(Socket)
    end,
    Reason.

connect(#state{host = Host, port = Port, timeout = Timeout, tube = Tube, monitor = NotificationPid, recon_interval = ReconnectionInterval, decoder_state = DecoderState} = State) ->
    case gen_tcp:connect(Host, Port, ?CONNECT_OPTIONS, Timeout) of
        {ok, Socket} ->
            ?LOG_INFO("connection completed: ~p", [Socket]),

            case update_tube(Socket, Tube, DecoderState) of
                {ok, NewDecoderState} ->
                    notification_connection_up(NotificationPid),
                    State#state{socket = Socket, decoder_state = NewDecoderState};
                TubeError ->
                    ?LOG_ERROR("failed to set the proper tube. error: ~p . attempt reconnection in ~p ms", [TubeError, ReconnectionInterval]),
                    erlang:send_after(ReconnectionInterval, self(), reconnect),
                    gen_tcp:close(Socket),
                    State#state{socket = undefined, decoder_state = ebeanstalkd_decoder:new()}
            end;
        Error ->
            ?LOG_ERROR("failed to connect. try again in ~p ms. error: ~p", [ReconnectionInterval, Error]),
            erlang:send_after(ReconnectionInterval, self(), reconnect),
            State#state{socket = undefined, decoder_state = ebeanstalkd_decoder:new()}
    end.

disconnect(#state{queue = Queue, monitor = MonitorRef} = State) ->
    clear_queue(Queue),
    notification_connection_down(MonitorRef),
    erlang:send_after(0, self(), reconnect),
    State#state{socket = undefined, queue = queue:new(), queue_length = 0, decoder_state = ebeanstalkd_decoder:new()}.

update_tube(Socket, TubeOption, DecoderState) ->
    case TubeOption of
        undefined ->
            {ok, DecoderState};
        _ ->
            case set_tube_option(Socket, TubeOption, DecoderState) of
                {ok, NewDecoderState} ->
                    maybe_ignore_default_tube(Socket, TubeOption, NewDecoderState);
                Result ->
                    Result
            end
    end.

set_tube_option(Socket, {_, Tube} = TubeCommand, DecoderState) when is_binary(Tube) ->
    set_tube(Socket, TubeCommand, DecoderState);

set_tube_option(Socket, {Command, [H|T]}, DecoderState) ->
    case set_tube(Socket, {Command, H}, DecoderState) of
        {ok, NewDecoderState} ->
            set_tube_option(Socket, {Command, T}, NewDecoderState);
        Result ->
            Result
    end;
set_tube_option(_Socket, {_Command, []}, DecoderState) ->
    {ok, DecoderState}.

set_tube(Socket, Tube, DecoderState) ->
    Cmd = case Tube of
        {watch, TubeName} ->
            ?BK_WATCH(TubeName);
        {use, TubeName} ->
            ?BK_USE(TubeName)
    end,

    case send_sync(Socket, Cmd, DecoderState) of
        {ok, Response, NewDecoderState} ->
            case Response of
                {watching, _} ->
                    {ok, NewDecoderState};
                {using, _} ->
                    {ok, NewDecoderState};
                Result ->
                    ?LOG_ERROR("failed to set tube: ~p result: ~p", [Tube, Response]),
                    {error, {unexpected_result, Result}}
            end;
        Result ->
            ?LOG_ERROR("failed to set tube: ~p result: ~p", [Tube, Result]),
            Result
    end.

ignore_tube(Socket, Tube, DecoderState) ->
    case send_sync(Socket, ?BK_IGNORE(Tube), DecoderState) of
        {ok, {watching, _}, NewDecoderState} ->
            {ok, NewDecoderState};
        Result ->
            ?LOG_ERROR("failed to ignore tube: ~p error: ~p", [Tube, Result]),
            Result
    end.

maybe_ignore_default_tube(Socket, {watch, TubeList}, DecoderState) when is_list(TubeList) ->
    case lists:member(?DEFAULT_TUBE_NAME, TubeList) of
        false ->
            ignore_tube(Socket, ?DEFAULT_TUBE_NAME, DecoderState);
        _ ->
            {ok, DecoderState}
    end;
maybe_ignore_default_tube(Socket, {watch, Tube}, DecoderState) when is_binary(Tube) ->
    case Tube of
        ?DEFAULT_TUBE_NAME ->
            {ok, DecoderState};
        _ ->
            ignore_tube(Socket, ?DEFAULT_TUBE_NAME, DecoderState)
    end;
maybe_ignore_default_tube(_Socket, _Tubes, DecoderState) ->
    {ok, DecoderState}.

send_sync(Socket, CommandPayload, DecoderState) ->
    case gen_tcp:send(Socket, ebeanstalkd_encoder:encode(CommandPayload)) of
        ok ->
            recv_and_decode_sync(Socket, DecoderState);
        Error ->
            Error
    end.

recv_and_decode_sync(Socket, DecoderState) ->
    receive
        {tcp, Socket, Packet} ->
            case ebeanstalkd_decoder:decode(Packet, DecoderState) of
                {ok, _, _} = R ->
                    R;
                {more, NewDecoderState} ->
                    recv_and_decode_sync(Socket, NewDecoderState)
            end;
        {tcp_closed, S} ->
            {tcp_closed, S}
    end.

decode_packet(Packet, #state{decoder_state = DecoderState, queue = Queue, queue_length = QueueLength} = State) ->
    case ebeanstalkd_decoder:decode(Packet, DecoderState) of
        {ok, X, NewDecoderState} ->
            {{value, {FromPid, Tag}}, Queue2} = queue:out(Queue),
            reply(FromPid, Tag, X),
            decode_packet(<<>>, State#state{queue = Queue2, queue_length = QueueLength-1, decoder_state = NewDecoderState});
        {more, NewDecoderState} ->
            {ok, State#state{decoder_state = NewDecoderState}};
        Error ->
            {error, Error, State}
    end.

clear_queue(Queue) ->
    clear_queue(Queue, {error, not_connected}).

clear_queue(Queue, Reason) ->
    FunNotifyPendingReq = fun({FromPid, Tag}) ->
        reply(FromPid, Tag, Reason)
    end,

    lists:foreach(FunNotifyPendingReq, queue:to_list(Queue)).

notification_connection_up(Pid)->
    send_notification(Pid, {connection_status, {up, self()}}).

notification_connection_down(Pid) ->
    send_notification(Pid, {connection_status, {down, self()}}).

send_notification(undefined, _Notification) ->
    ok;
send_notification(Pid, Notification) ->
    Pid ! Notification.

reply(undefined, undefined, _Response) ->
    ok;
reply(Pid, Tag, Response) ->
    Pid ! {response, Tag, Response}.
