-module(load_test).

-export([
    run/4
]).

run(Profiling, ClientsNr, ReqNr, PayloadLength) ->
    application:ensure_all_started(ebeanstalkd),

    start_profiling(Profiling),

    ReqPerProcess = round(ReqNr/ClientsNr),
    Message = <<0:PayloadLength/little-signed-integer-unit:8>>,

    Self = self(),
    A = os:timestamp(),
    Pids = [spawn_link(fun() -> loop(ReqPerProcess, Message), Self ! {self(), done} end) || _ <- lists:seq(1, ClientsNr)],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),
    stop_profiling(Profiling),

    TimeMs = timer:now_diff(B, A)/1000,
    TimeS = TimeMs/1000,
    io:format("## Test complete time: ~p ms => throughput ~p/s ~n", [TimeMs, round(ReqNr/TimeS, 2)]),
    ok.

% internals

start_profiling(true) ->
    {ok, P} = eprof:start(),
    eprof:start_profiling(processes() -- [P]),
    ok;
start_profiling(_) ->
    false.

stop_profiling(true) ->
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop(),
    ok;
stop_profiling(_) ->
    ok.

loop(0, _) ->
    ok;
loop(ReqNr, Message) ->
    case ebeanstalkd:put(bk_pool, Message) of
        {inserted, _} ->
            ok;
        Rs ->
            io:format("unexpected response for job: ~p pid: ~p response: ~p ~n", [ReqNr, self(), Rs]),
            throw(Rs)
    end,
    loop(ReqNr -1, Message).

round(null, _Precision) ->
    null;
round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.
