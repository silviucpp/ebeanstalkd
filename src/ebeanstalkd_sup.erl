-module(ebeanstalkd_sup).

-include("ebeanstalkd.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    FunPool = fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}}, {worker_module, ebeanstalkd_connection}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end,

    PoolSpecs = lists:map(FunPool, get_pools()),
    {ok, {{one_for_one, 100, 10}, PoolSpecs}}.

get_pools() ->
    case ebeanstalkd_utils:env(pools) of
        undefined ->
            [];
        Value ->
            Value
    end.