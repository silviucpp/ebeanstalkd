-module(ebeanstalkd).

-include("ebeanstalkd.hrl").

-behaviour(application).

-export([start/2, stop/1]).

-export([connect/0, connect/1, close/1,
         put_in_tube/3, put_in_tube/4, put/2, put/3,
         use/2, watch/2, ignore/2,
         reserve/1, reserve/2, delete/2, release/2, release/3, bury/2, bury/3, touch/2,
         peek/2, peek_ready/1, peek_buried/1, peek_delayed/1, kick/2, kick_job/2,
         stats_job/2, stats_tube/2, stats/1, list_tubes/1, list_tube_used/1, list_tubes_watched/1]).

start(_Type, _Args) ->
    ebeanstalkd_sup:start_link().

stop(_State) ->
    ok.

connect() ->
    connect([]).
connect(Options) ->
    ebeanstalkd_connection:start_link(Options).

close(Pid) ->
    ebeanstalkd_connection:stop(Pid).

put_in_tube(InstanceRef, Tube, Data) ->
    put_in_tube(InstanceRef, Tube, Data, []).

put_in_tube(InstanceRef, Tube, Data, Options) ->
    case use(InstanceRef, Tube) of
        {using, _} ->
            put(InstanceRef, Data, Options);
        UnexpectedError ->
            UnexpectedError
    end.

put(InstanceRef, Data) ->
    ebeanstalkd:put(InstanceRef, Data, []).

put(InstanceRef, Data, Params) ->
    Pri = ebeanstalkd_utils:lookup(pri, Params, ?DEFAULT_PRIORITY),
    Delay = ebeanstalkd_utils:lookup(delay, Params, ?DEFAULT_DELAY),
    TTR = ebeanstalkd_utils:lookup(ttr, Params, ?DEFAULT_TTR),
    bk_exec(InstanceRef, ?BK_PUT(Data, Pri, Delay, TTR, size(Data))).

use(InstanceRef, Tube) when is_binary(Tube) ->
    bk_exec(InstanceRef, ?BK_USE(Tube)).

reserve(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_RESERVE(), infinity).

reserve(InstanceRef, Timeout) ->
    bk_exec(InstanceRef, ?BK_RESERVE(Timeout), infinity).

delete(InstanceRef, Id) ->
    bk_exec(InstanceRef, ?BK_DELETE(Id)).

release(InstanceRef, Id) ->
    release(InstanceRef, Id, []).

release(InstanceRef, ID, Params) ->
    Pri = ebeanstalkd_utils:lookup(pri, Params, ?DEFAULT_PRIORITY),
    Delay = ebeanstalkd_utils:lookup(delay, Params, ?DEFAULT_DELAY),
    bk_exec(InstanceRef, ?BK_RELEASE(ID, Pri, Delay)).

bury(InstanceRef, ID) ->
    bury(InstanceRef, ID, 0).

bury(InstanceRef, ID, Priority) ->
    bk_exec(InstanceRef, ?BK_BURY(ID, Priority)).

touch(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_TOUCH(ID)).

watch(InstanceRef, Tube) when is_binary(Tube) ->
    bk_exec(InstanceRef, ?BK_WATCH(Tube)).

ignore(InstanceRef, Tube) when is_binary(Tube) ->
    bk_exec(InstanceRef, ?BK_IGNORE(Tube)).

peek(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_PEEK(ID)).

peek_ready(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_PEEK_READY()).

peek_delayed(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_PEEK_DELAYED()).

peek_buried(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_PEEK_BURIED()).

kick(InstanceRef, Bound) ->
    bk_exec(InstanceRef, ?BK_KICK(Bound)).

kick_job(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_KICK_JOB(ID)).

stats_job(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_STATS_JOB(ID)).

stats_tube(InstanceRef, Tube) when is_binary(Tube) ->
    bk_exec(InstanceRef, ?BK_STATS_TUBE(Tube)).

stats(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_STATS()).

list_tubes(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_LIST_TUBES()).

list_tube_used(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_LIST_TUBE_USED()).

list_tubes_watched(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_LIST_TUBES_WATCHED()).

%internals

bk_exec(InstanceRef, Msg) ->
    bk_exec(InstanceRef, Msg, 20000).

bk_exec(InstanceRef, Msg, Timeout) ->
    Tag = make_ref(),

    case is_atom(InstanceRef) of
        true ->
            poolboy:transaction(InstanceRef, fun(Pid) -> Pid ! {command, self(), Tag, Msg} end);
        _ ->
            InstanceRef ! {command, self(), Tag, Msg}
    end,

    receive
        {response, Tag, Response} ->
            Response
    after Timeout ->
        {error, timeout}
    end.