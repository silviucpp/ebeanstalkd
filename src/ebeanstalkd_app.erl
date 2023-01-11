-module(ebeanstalkd_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    ok = start_pools(),
    ebeanstalkd_sup:start_link().

stop(_State) ->
    erlpool:stop_group(ebeanstalkd),
    ok.

start_pools() ->
    FunPool = fun({Name, Args}) -> ok = ebeanstalkd:start_pool(Name, Args) end,
    lists:foreach(FunPool, get_pools()).

get_pools() ->
    case ebeanstalkd_utils:env(pools) of
        undefined ->
            [];
        Value ->
            Value
    end.
