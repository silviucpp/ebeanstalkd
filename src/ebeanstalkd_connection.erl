-module(ebeanstalkd_connection).

-include("ebeanstalkd.hrl").

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

-behaviour(gen_server).

-export([
    start_link/1,
    stop/1,
    command/3,
    batch_command/3,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
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

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

stop(Pid) ->
    gen_server:stop(Pid).

command(Pid, CommandPayload, Timeout) ->
    ebeanstalkd_utils:safe_call(Pid, {command, CommandPayload}, Timeout).

batch_command(Pid, Commands, Timeout) ->
    ebeanstalkd_utils:safe_call(Pid, {batch_command, Commands}, Timeout).

init(Options) ->
    Host = ebeanstalkd_utils:lookup(host, Options, ?DEFAULT_IP),
    Port = ebeanstalkd_utils:lookup(port, Options, ?DEFAULT_PORT),
    Timeout = ebeanstalkd_utils:lookup(timeout, Options, ?DEFAULT_CONNECTION_TIMEOUT_MS),
    Tube = ebeanstalkd_utils:lookup(tube, Options, ?DEFAULT_TUBE),
    RecInterval = ebeanstalkd_utils:lookup(reconnect_interval, Options, ?RECONNECT_MS),
    Monitor = ebeanstalkd_utils:lookup(monitor, Options, undefined),

    {ok, connect(#state{
        host = Host,
        port = Port,
        tube = Tube,
        timeout = Timeout,
        recon_interval = RecInterval,
        monitor = Monitor,
        queue = queue:new(),
        queue_length = 0,
        decoder_state = ebeanstalkd_decoder:new()
    })}.

handle_call({command, CommandPayload}, From, #state{socket = Socket, queue_length = QueueLength, queue = Queue} = State) ->
    case Socket of
        undefined ->
            {reply, {error, not_connected}, State};
        _ ->
            case QueueLength > ?MAX_PENDING_REQUESTS_QUEUE of
                true ->
                    {reply, {error, full_queue}, State};
                _ ->
                    case gen_tcp:send(Socket, ebeanstalkd_encoder:encode(CommandPayload)) of
                        ok ->
                            {noreply, State#state{queue = queue:in(From, Queue), queue_length = QueueLength + 1}};
                        UnexpectedResult ->
                            {reply, UnexpectedResult, disconnect(State)}
                    end
            end
    end;
handle_call({batch_command, Commands}, From, #state{socket = Socket, queue_length = QueueLength, queue = Queue} = State) ->
    case Socket of
        undefined ->
            {reply, {error, not_connected}, State};
        _ ->
            case QueueLength > ?MAX_PENDING_REQUESTS_QUEUE of
                true ->
                    {reply, {error, full_queue}, State};
                _ ->
                    EncodedPayload = lists:map(fun(C) -> ebeanstalkd_encoder:encode(C) end, Commands),

                    case gen_tcp:send(Socket, EncodedPayload) of
                        ok ->
                            Length = length(Commands),
                            Queue1 = lists:foldl(fun(_, Q) -> queue:in(undefined, Q) end, Queue, lists:seq(1, Length - 1)),
                            {noreply, State#state{queue = queue:in(From, Queue1), queue_length = QueueLength + Length}};
                        UnexpectedResult ->
                            {reply, UnexpectedResult, disconnect(State)}
                    end
            end
    end;
handle_call(Request, _From, State) ->
    ?LOG_ERROR("handle_call unhandled request : ~p", [Request]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("handle_cast unhandled request : ~p", [Request]),
    {noreply, State}.

handle_info({tcp, _Socket, Packet}, State) ->
    NewState = case decode_packet(Packet, State) of
        {ok, NewState0} ->
            NewState0;
        {error, ErrorMsg, NewState0} ->
           ?LOG_WARNING("failed to parse protocol: ~p", [ErrorMsg]),
           disconnect(NewState0)
    end,
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {noreply, disconnect(State)};
handle_info(reconnect, #state{host = Host, port = Port} = State) ->
    ?LOG_INFO("try to reconnect to ip: ~p port: ~p", [Host, Port]),
    {noreply, connect(State)};
handle_info(Info, State) ->
    ?LOG_ERROR("handle_info unhandled message : ~p", [Info]),
    {noreply, State}.

terminate(Reason, #state{socket = Socket, queue = Queue}) ->
    case Reason of
        normal ->
            clear_queue(Queue, {error, connection_closed});
        _ ->
            clear_queue(Queue, {error, Reason})
    end,

    case Socket of
        undefined ->
            ok;
        _ ->
            catch gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internals

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
            {{value, From}, Queue2} = queue:out(Queue),
            safe_reply(From, X),
            decode_packet(<<>>, State#state{queue = Queue2, queue_length = QueueLength-1, decoder_state = NewDecoderState});
        {more, NewDecoderState} ->
            {ok, State#state{decoder_state = NewDecoderState}};
        Error ->
            {error, Error, State}
    end.

clear_queue(Queue) ->
    clear_queue(Queue, {error, not_connected}).

clear_queue(Queue, Reason) ->
    lists:foreach(fun(From) -> safe_reply(From, Reason) end, queue:to_list(Queue)).

notification_connection_up(Pid)->
    send_notification(Pid, {connection_status, {up, self()}}).

notification_connection_down(Pid) ->
    send_notification(Pid, {connection_status, {down, self()}}).

send_notification(undefined, _Notification) ->
    ok;
send_notification(Pid, Notification) ->
    Pid ! Notification.

safe_reply(undefined, _Response) ->
    ok;
safe_reply(From, Response) ->
    gen_server:reply(From, Response).
