-module(load_test).
-author("silviu.caragea").

-export([run/3]).

loop(0) ->
    ok;
loop(ReqNr) ->
     case ebeanstalkd:put(bk_pool, integer_to_binary(ReqNr)) of
         {inserted, _} ->
             ok;
         Rs ->
             io:format(<<"unexpected response for job: ~p pid: ~p ~n">>, [ReqNr, self()]),
             throw(Rs)
     end,

    loop(ReqNr -1).

run(plist, ClientsNr, ReqNr) ->
    ReqPerProcess = round(ReqNr/ClientsNr),
    Fun = fun(_X)-> loop(ReqPerProcess) end,
    do_load(plist, Fun, ClientsNr);

run(multispawn, ClientsNr, ReqNr) ->
    ReqPerProcess = round(ReqNr/ClientsNr),

    io:format(<<"req per process: ~p ~n">>, [ReqPerProcess]),
    do_load(multispawn, fun()-> loop(ReqPerProcess) end, ClientsNr).

do_load(Method, Fun, ClientsNr) ->
    application:ensure_all_started(ebeanstalkd),

    %{ok, P} = eprof:start(),
    %eprof:start_profiling(processes() -- [P]),

    {Time, _} = case Method of
        plist ->
            List = lists:seq(1, ClientsNr),
            timer:tc(fun()-> plists:foreach(Fun, List, {processes, schedulers}) end);
        multispawn ->
            timer:tc(fun()-> multi_spawn:do_work(Fun, ClientsNr) end)
    end,

    %eprof:stop_profiling(),
    %eprof:analyze(total),
    %eprof:stop(),

    Time/1000.