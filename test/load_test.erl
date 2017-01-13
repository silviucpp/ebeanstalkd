-module(load_test).
-author("silviu.caragea").

-export([run/4]).

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

run(Profiling, ClientsNr, ReqNr, PayloadLength) ->
    application:ensure_all_started(ebeanstalkd),

    start_profiling(Profiling),

    ReqPerProcess = round(ReqNr/ClientsNr),
    Message = <<0:PayloadLength/little-signed-integer-unit:8>>,

    FunTest = fun()-> loop(ReqPerProcess, Message) end,
    {Time, _} = timer:tc(fun()-> multi_spawn:do_work(FunTest, ClientsNr) end),
    stop_profiling(Profiling),

    TimeMs = Time/1000,
    TimeS = TimeMs/1000,
    io:format("## Test complete time: ~p ms => throughput ~p/s ~n", [TimeMs, ReqNr/TimeS]),
    ok.

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
