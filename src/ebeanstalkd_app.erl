-module(ebeanstalkd_app).
-author("silviu.caragea").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ebeanstalkd_sup:start_link().

stop(_State) ->
    ok.
