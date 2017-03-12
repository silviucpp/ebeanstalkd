-module(ebeanstalkd_app).
-author("silviu.caragea").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = start_pools(),
    ebeanstalkd_sup:start_link().

stop(_State) ->
    ok.

start_pools() ->
    FunPool = fun({Name, Args0}) ->
        Size = ebeanstalkd_utils:lookup(size, Args0, undefined),
        ok = erlpool:start_pool(Name, [
            {size, Size},
            {start_mfa, {ebeanstalkd_connection, start_link, [lists:keydelete(size, 1, Args0)]}}
        ])
    end,
    lists:foreach(FunPool, get_pools()).

get_pools() ->
    case ebeanstalkd_utils:env(pools) of
        undefined ->
            [];
        Value ->
            Value
    end.