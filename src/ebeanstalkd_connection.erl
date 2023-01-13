-module(ebeanstalkd_connection).

-include("ebeanstalkd.hrl").

-export([
    start_link/1,
    stop/1,
    init/2
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
    buff
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
    Host = ebeanstalkd_utils:lookup(host, Options, ?DEFAULT_IP),
    Port = ebeanstalkd_utils:lookup(port, Options, ?DEFAULT_PORT),
    Timeout = ebeanstalkd_utils:lookup(timeout, Options, ?DEFAULT_CONNECTION_TIMEOUT_MS),
    Tube = ebeanstalkd_utils:lookup(tube, Options, ?DEFAULT_TUBE),
    RecInterval = ebeanstalkd_utils:lookup(reconnect_interval, Options, ?RECONNECT_MS),
    Monitor = ebeanstalkd_utils:lookup(monitor, Options, undefined),
    Socket = connect(Host, Port, Timeout, Tube, RecInterval, Monitor),

    proc_lib:init_ack(Parent, {ok, self()}),

    connection_loop(#state{
        socket = Socket,
        host = Host,
        port = Port,
        tube = Tube,
        timeout = Timeout,
        recon_interval = RecInterval,
        monitor = Monitor,
        queue = queue:new(),
        queue_length = 0,
        buff = <<>>
    }).

connection_loop(State) ->
    try
        receive
            {command, From, Tag, CommandPayload} ->
                connection_loop(send_command(From, Tag, State, CommandPayload));
            {batch_command, From, Tag, Commands} ->
                connection_loop(send_batch(From, Tag, State, Commands));
            {tcp, _Socket, Packet} ->
                connection_loop(receive_async(Packet, State));
            {tcp_closed, _Socket} ->
                connection_loop(disconnect(State));
            reconnect ->
                connection_loop(reconnect(State));
            stop ->
                terminate(normal, State);
            UnexpectedMessage ->
                ?WARNING_MSG("received unexpected message: ~p", [UnexpectedMessage]),
                connection_loop(State)
        end
    catch
        ?EXCEPTION(_, Error, Stacktrace) ->
            ?WARNING_MSG("exception received: ~p stack: ~p", [Error, ?GET_STACK(Stacktrace)]),
            terminate(Error, State)
    end.

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
                    case send(Socket, Command, true) of
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

reconnect(#state{
    host = Host,
    port = Port,
    timeout = Timeout,
    tube = Tube,
    recon_interval = ReconInterval,
    monitor = MonitorRef
} = State) ->
    ?INFO_MSG("try to reconnect to ip: ~p port: ~p", [Host, Port]),
    Socket = connect(Host, Port, Timeout, Tube, ReconInterval, MonitorRef),
    State#state{socket = Socket}.

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
    end.

connect(Host, Port, Timeout, Tube, ReconnectionInterval, NotificationPid) ->

    case gen_tcp:connect(Host, Port, ?CONNECT_OPTIONS, Timeout) of
        {ok, Socket} ->
            ?INFO_MSG("connection completed: ~p", [Socket]),

            case update_tube(Socket, Tube) of
                ok ->
                    notification_connection_up(NotificationPid),
                    Socket;
                TubeError ->
                    ?ERROR_MSG("failed to set the proper tube. error: ~p . attempt reconnection in ~p ms", [TubeError, ReconnectionInterval]),
                    erlang:send_after(ReconnectionInterval, self(), reconnect),
                    undefined
            end;
        Error ->
            ?ERROR_MSG("failed to connect. try again in ~p ms. error: ~p", [ReconnectionInterval, Error]),
            erlang:send_after(ReconnectionInterval, self(), reconnect),
            undefined
    end.

disconnect(#state{queue = Queue, monitor = MonitorRef} = State) ->
    clear_queue(Queue),
    notification_connection_down(MonitorRef),
    erlang:send_after(0, self(), reconnect),
    State#state{socket = undefined, queue = queue:new(), queue_length = 0, buff = <<>>}.

update_tube(Socket, TubeOption) ->
    case TubeOption of
        undefined ->
            ok;
        _ ->
            case set_tube_option(Socket, TubeOption) of
                ok ->
                    maybe_ignore_default_tube(Socket, TubeOption);
                Result ->
                    Result
            end
    end.

set_tube_option(Socket, {_, Tube} = TubeCommand) when is_binary(Tube) ->
    set_tube(Socket, TubeCommand);

set_tube_option(Socket, {Command, [H|T]}) ->
    case set_tube(Socket, {Command, H}) of
        ok ->
            set_tube_option(Socket, {Command, T});
        Result ->
            Result
    end;
set_tube_option(_Socket, {_Command, []}) ->
    ok.

set_tube(Socket, Tube) ->
    Cmd = case Tube of
        {watch, TubeName} ->
            ?BK_WATCH(TubeName);
        {use, TubeName} ->
            ?BK_USE(TubeName)
    end,

    case send(Socket, Cmd, false) of
        {ok, {watching, _}} ->
            ok;
        {ok, {using, _}} ->
            ok;
        Result ->
            ?ERROR_MSG("failed to set tube: ~p result: ~p", [Tube, Result]),
            Result
    end.

ignore_tube(Socket, Tube) ->
    case send(Socket, ?BK_IGNORE(Tube), false) of
        {ok, {watching, _}} ->
            ok;
        Result ->
            ?ERROR_MSG("failed to ignore tube: ~p error: ~p", [Tube, Result]),
            Result
    end.

maybe_ignore_default_tube(Socket, {watch, TubeList}) when is_list(TubeList) ->
    case lists:member(?DEFAULT_TUBE_NAME, TubeList) of
        false ->
            ignore_tube(Socket, ?DEFAULT_TUBE_NAME);
        _ ->
            ok
    end;
maybe_ignore_default_tube(Socket, {watch, Tube}) when is_binary(Tube) ->
    case Tube of
        ?DEFAULT_TUBE_NAME ->
            ok;
        _ ->
            ignore_tube(Socket, ?DEFAULT_TUBE_NAME)
    end;
maybe_ignore_default_tube(_Socket, _Tubes) ->
    ok.

send(Socket, CommandPayload, Async) ->
    SendResult = gen_tcp:send(Socket, ebeanstalkd_encoder:encode(CommandPayload)),

    case Async of
        true ->
            SendResult;
        _ ->
            case SendResult of
                ok ->
                    recv(Socket);
                _ ->
                    SendResult
            end
    end.

recv(Socket) ->
    recv(Socket, <<>>).

recv(Socket, Data) ->
    receive
        {tcp, Socket, Packet} ->
            NewData = <<Data/binary, Packet/binary>>,
            case ebeanstalkd_decoder:decode(NewData) of
                more ->
                    recv(Socket, NewData);
                {ok, X, <<>>} ->
                    {ok, X}
            end;
        {tcp_closed, S} ->
            {tcp_closed, S}
    end.

receive_async(Packet, #state{buff = ExistingBuffer, queue = Queue, queue_length = QueueLength} = State) ->
    NewData = <<ExistingBuffer/binary, Packet/binary>>,

    case ebeanstalkd_decoder:decode(NewData) of
        more ->
            State#state{buff = NewData};
        {ok, X, Rest} ->
            {{value, {FromPid, Tag}}, Queue2} = queue:out(Queue),
            reply(FromPid, Tag, X),

            case Rest of
                <<>> ->
                    State#state{queue = Queue2, queue_length = QueueLength - 1, buff = <<>>};
                _ ->
                    receive_async(Rest, State#state{buff = <<>>, queue = Queue2, queue_length = QueueLength - 1})
            end
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
