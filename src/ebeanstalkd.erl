-module(ebeanstalkd).

-include("ebeanstalkd.hrl").

-export([
    start/0,
    start/1,
    stop/0,
    start_pool/2,
    stop_pool/1,
    connect/0,
    connect/1,
    close/1,
    put_in_tube/3,
    put_in_tube/4,
    put_in_tube2/3,
    put_in_tube2/4,
    put/2,
    put/3,
    use/2,
    watch/2,
    ignore/2,
    reserve/1,
    reserve/2,
    delete/2,
    release/2,
    release/3,
    bury/2,
    bury/3,
    touch/2,
    peek/2,
    peek_ready/1,
    peek_buried/1,
    peek_delayed/1,
    kick/2,
    kick_job/2,
    stats_job/2,
    stats_tube/2,
    stats/1,
    list_tubes/1,
    list_tube_used/1,
    list_tubes_watched/1
]).

-spec start() ->
    ok | {error, reason()}.

start() ->
    start(temporary).

-spec start(permanent | transient | temporary) ->
    ok | {error, reason()}.

start(Type) ->
    case application:ensure_all_started(ebeanstalkd, Type) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end.

-spec stop() ->
    ok.

stop() ->
    application:stop(ebeanstalkd).

-spec start_pool(atom(), [pool_option()]) ->
    ok | {error, reason()}.

start_pool(PoolName, PoolArgs0) ->
    Size = ebeanstalkd_utils:lookup(size, PoolArgs0, undefined),
    ok = erlpool:start_pool(PoolName, [
        {size, Size},
        {group, ebeanstalkd},
        {start_mfa, {ebeanstalkd_connection, start_link, [lists:keydelete(size, 1, PoolArgs0)]}}
    ]).

-spec stop_pool(atom()) ->
    ok | {error, reason()}.

stop_pool(PoolName) ->
    erlpool:stop_pool(PoolName).

-spec connect() ->
    {ok, pid()} | {error, reason()}.

connect() ->
    connect([]).

-spec connect([connect_option()]) ->
    {ok, pid()} | {error, reason()}.

connect(Options) ->
    ebeanstalkd_connection:start_link(Options).

-spec close(pid()) ->
    ok | {error, reason()}.

close(Pid) ->
    ebeanstalkd_connection:stop(Pid).

-spec put_in_tube(con_ref(), tube(), binary()) ->
    {inserted, job_id()} | {error, reason()}.

put_in_tube(InstanceRef, Tube, Data) ->
    put_in_tube(InstanceRef, Tube, Data, []).

-spec put_in_tube(con_ref(), tube(), binary(), [put_option()]) ->
    {inserted, job_id()} | {error, reason()}.

put_in_tube(InstanceRef, Tube, Data, Params) ->
    %we send both commands in pipeline and only last one will receive the ack
    %based on kr here this should be fine https://github.com/kr/beanstalkd/issues/332
    %even if I don't totally agree
    Pri = ebeanstalkd_utils:lookup(pri, Params, ?DEFAULT_PRIORITY),
    Delay = ebeanstalkd_utils:lookup(delay, Params, ?DEFAULT_DELAY),
    TTR = ebeanstalkd_utils:lookup(ttr, Params, ?DEFAULT_TTR),
    Cmds = [?BK_USE(Tube), ?BK_PUT(Data, Pri, Delay, TTR, size(Data))],
    bk_exec(InstanceRef, {batch, Cmds}).

-spec put_in_tube2(con_ref(), tube(), binary()) ->
    {inserted, job_id()} | {error, reason()}.

put_in_tube2(InstanceRef, Tube, Data) ->
    put_in_tube2(InstanceRef, Tube, Data, []).

-spec put_in_tube2(con_ref(), tube(), binary(), [put_option()]) ->
    {inserted, job_id()} | {error, reason()}.

put_in_tube2(InstanceRef, Tube, Data, Params) ->
    Pri = ebeanstalkd_utils:lookup(pri, Params, ?DEFAULT_PRIORITY),
    Delay = ebeanstalkd_utils:lookup(delay, Params, ?DEFAULT_DELAY),
    TTR = ebeanstalkd_utils:lookup(ttr, Params, ?DEFAULT_TTR),
    bk_exec(InstanceRef, ?BK_PUT_IN_TUBE(Tube, Data, Pri, Delay, TTR, size(Data))).

-spec put(con_ref(), binary()) ->
    {inserted, job_id()} | {error, reason()}.

put(InstanceRef, Data) ->
    ebeanstalkd:put(InstanceRef, Data, []).

-spec put(con_ref(), binary(), [put_option()]) ->
    {inserted, job_id()} | {error, reason()}.

put(InstanceRef, Data, Params) ->
    Pri = ebeanstalkd_utils:lookup(pri, Params, ?DEFAULT_PRIORITY),
    Delay = ebeanstalkd_utils:lookup(delay, Params, ?DEFAULT_DELAY),
    TTR = ebeanstalkd_utils:lookup(ttr, Params, ?DEFAULT_TTR),
    bk_exec(InstanceRef, ?BK_PUT(Data, Pri, Delay, TTR, size(Data))).

-spec use(con_ref(), tube()) ->
    {using, tube()} | {error, reason()}.

use(InstanceRef, Tube) when is_binary(Tube) ->
    bk_exec(InstanceRef, ?BK_USE(Tube)).

-spec reserve(con_ref()) ->
    {reserved, job_id(), binary()} | {error, reason()}.

reserve(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_RESERVE(), infinity).

-spec reserve(con_ref(), timeout()) ->
    {reserved, job_id(), binary()} | {error, reason()}.

reserve(InstanceRef, Timeout) ->
    bk_exec(InstanceRef, ?BK_RESERVE(Timeout), (Timeout*1000 + 2000)).

-spec delete(con_ref(), job_id()) ->
    {deleted} | {error, reason()}.

delete(InstanceRef, Id) ->
    bk_exec(InstanceRef, ?BK_DELETE(Id)).

-spec release(con_ref(), job_id()) ->
    {released} | {error, reason()}.

release(InstanceRef, Id) ->
    release(InstanceRef, Id, []).

-spec release(con_ref(), job_id(), [release_option()]) ->
    {released} | {error, reason()}.

release(InstanceRef, ID, Params) ->
    Pri = ebeanstalkd_utils:lookup(pri, Params, ?DEFAULT_PRIORITY),
    Delay = ebeanstalkd_utils:lookup(delay, Params, ?DEFAULT_DELAY),
    bk_exec(InstanceRef, ?BK_RELEASE(ID, Pri, Delay)).

-spec bury(con_ref(), job_id()) ->
    {buried} | {error, reason()}.

bury(InstanceRef, ID) ->
    bury(InstanceRef, ID, 0).

-spec bury(con_ref(), job_id(), non_neg_integer()) ->
    {buried} | {error, reason()}.

bury(InstanceRef, ID, Priority) ->
    bk_exec(InstanceRef, ?BK_BURY(ID, Priority)).

-spec touch(con_ref(), job_id()) ->
    {touched} | {error, reason()}.

touch(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_TOUCH(ID)).

-spec watch(con_ref(), tube()) ->
    {watching, [tube()]} | {error, reason()}.

watch(InstanceRef, Tube) ->
    bk_exec(InstanceRef, ?BK_WATCH(Tube)).

-spec ignore(con_ref(), tube()) ->
    {watching, [tube()]} | {error, reason()}.

ignore(InstanceRef, Tube) ->
    bk_exec(InstanceRef, ?BK_IGNORE(Tube)).

-spec peek(con_ref(), job_id()) ->
    {found, job_id(), binary()} | {error, reason()}.

peek(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_PEEK(ID)).

-spec peek_ready(con_ref()) ->
    {found, job_id(), binary()} | {error, reason()}.

peek_ready(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_PEEK_READY()).

-spec peek_delayed(con_ref()) ->
    {found, job_id(), binary()} | {error, reason()}.

peek_delayed(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_PEEK_DELAYED()).

-spec peek_buried(con_ref()) ->
    {found, job_id(), binary()} | {error, reason()}.

peek_buried(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_PEEK_BURIED()).

-spec kick(con_ref(), non_neg_integer()) ->
    {kicked, non_neg_integer()} | {error, reason()}.

kick(InstanceRef, Bound) ->
    bk_exec(InstanceRef, ?BK_KICK(Bound)).

-spec kick_job(con_ref(), job_id()) ->
    {kicked} | {error, reason()}.

kick_job(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_KICK_JOB(ID)).

-spec stats_job(con_ref(), job_id()) ->
    {ok, list()} | {error, reason()}.

stats_job(InstanceRef, ID) ->
    bk_exec(InstanceRef, ?BK_STATS_JOB(ID)).

-spec stats_tube(con_ref(), tube()) ->
    {ok, list()} | {error, reason()}.

stats_tube(InstanceRef, Tube) ->
    bk_exec(InstanceRef, ?BK_STATS_TUBE(Tube)).

-spec stats(con_ref()) ->
    {ok, list()} | {error, reason()}.

stats(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_STATS()).

-spec list_tubes(con_ref()) ->
    {ok, [tube()]} | {error, reason()}.

list_tubes(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_LIST_TUBES()).

-spec list_tube_used(con_ref()) ->
    {ok, [tube()]} | {error, reason()}.

list_tube_used(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_LIST_TUBE_USED()).

-spec list_tubes_watched(con_ref()) ->
    {ok, [tube()]} | {error, reason()}.

list_tubes_watched(InstanceRef) ->
    bk_exec(InstanceRef, ?BK_LIST_TUBES_WATCHED()).

%internals

bk_exec(InstanceRef, Msg) ->
    bk_exec(InstanceRef, Msg, ?DEFAULT_WAIT_RESPONSE_TIMEOUT_MS).

bk_exec(InstanceRef, Msg, Timeout) ->
    Tag = make_ref(),
    get_pid(InstanceRef) ! get_msg(Msg, self(), Tag),

    receive
        {response, Tag, Response} ->
            Response
    after Timeout ->
        {error, timeout}
    end.

get_msg({batch, CmdList}, Pid, Tag) ->
    {batch_command, Pid, Tag, CmdList};
get_msg(Cmd, Pid, Tag) ->
    {command, Pid, Tag, Cmd}.

get_pid(InstanceRef) when erlang:is_atom(InstanceRef) ->
    erlpool:pid(InstanceRef);
get_pid(InstanceRef) ->
    InstanceRef.
