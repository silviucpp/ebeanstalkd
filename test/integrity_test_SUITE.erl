-module(integrity_test_SUITE).

-include("ebeanstalkd.hrl").

%% note: you need to run the test on a clean beanstalkd server instance to make sure
%% jobs are not already exist (part of the tests might fail if jobs are already in tubes)

-compile(export_all).

all() -> [
    {group, ebeanstalkd_group}
].

groups() -> [
    {ebeanstalkd_group, [sequence], [
        test_pool,
        test_crash,
        test_parent_crashing,
        test_unexpected_msg,
        test_reconnect,
        test_put,
        test_use_watch_reserve_ignore_release_delete,
        test_reserve_timeout,
        test_bury_peek,
        test_touch,
        test_kick,
        test_stats_job,
        test_stats_tube,
        test_list,
        % might fail because of: https://github.com/beanstalkd/beanstalkd/commit/4c275d5945299e4562389f9f2ca7c326173d6335 not being released
        test_stats,
        test_put_in_tube2,
        test_capabilities
    ]}
].

init_per_suite(Config) ->
    application:ensure_all_started(ebeanstalkd),
    Config.

end_per_suite(_Config) ->
    ok.

test_pool(_Config) ->
    {inserted, Jb1} = ebeanstalkd:put(bk_pool, <<"job1">>),
    {deleted} = ebeanstalkd:delete(bk_pool, Jb1),
    ok = ebeanstalkd:stop_pool(bk_pool).

test_crash(_Config) ->
    process_flag(trap_exit, true),
    {ok, Q} = ebeanstalkd:connect([{tube, {watch, [<<"test_crash">>]}}]),
    spawn(fun() -> timer:sleep(1000), Q!{tcp, null, null} end),
    {error,{badarg, StackTrace}} = ebeanstalkd:reserve(Q),
    GotExit = receive
        {'EXIT', Q, {badarg, StackTrace}} ->
            true
        after 2000 ->
            false
    end,
    true = GotExit,
    false = is_process_alive(Q),
    process_flag(trap_exit, false).

test_parent_crashing(_Config) ->
    Parent = self(),
    spawn(fun() -> {ok, Q} = ebeanstalkd:connect(), Parent! {conn, Q}, exit(dummy_error) end),
    receive
        {conn, Q} ->
            ok = wait_alive(Q, 10)
        after 3000 ->
            throw(timeout)
    end.

test_unexpected_msg(_Config) ->
    {ok, Q} = ebeanstalkd:connect([{tube, {use, <<"test_unexpected_msg">>}}]),
    spawn(fun() -> Q!{unexpected_msg} end),
    ok = use_tube(Q, <<"test_unexpected_msg">>),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"test_put_PACKET1">>),
    {deleted} = ebeanstalkd:delete(Q, Jb1),
    ok = ebeanstalkd:close(Q).

test_reconnect(_Config) ->
    {ok, Q} = ebeanstalkd:connect([{monitor, self()},{tube, {use, <<"test_reconnect">>}}]),

    receive
        {connection_status, {up, Q}} ->
            spawn(fun() -> timer:sleep(2000), Q!{tcp_closed, null} end)
        after 5000 ->
            throw(timeout)
    end,

    {error, not_connected} = ebeanstalkd:reserve(Q),

    Results = lists:map(fun(_) ->
        receive
            {connection_status, {Status, Q}} ->
                Status
            after 5000 ->
                timeout
        end
    end, lists:seq(1, 2)),
    [down, up] = Results,
    ok = ebeanstalkd:close(Q).

test_put(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_put">>),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"test_put_PACKET1">>),
    {inserted, Jb2} = ebeanstalkd:put(Q, <<"test_put_PACKET2">>, [{pri, 0}, {delay, 0}, {ttr, 60}]),
    {inserted, Jb3} = ebeanstalkd:put_in_tube(Q, <<"test_put">>, <<"test_put_PACKET3">>),
    {inserted, Jb4} = ebeanstalkd:put_in_tube(Q, <<"test_put">>, <<"test_put_PACKET4">>, [{pri, 0}, {delay, 0}, {ttr, 60}]),
    {deleted} = ebeanstalkd:delete(Q, Jb1),
    {deleted} = ebeanstalkd:delete(Q, Jb2),
    {deleted} = ebeanstalkd:delete(Q, Jb3),
    {deleted} = ebeanstalkd:delete(Q, Jb4),
    ok = ebeanstalkd:close(Q).

test_use_watch_reserve_ignore_release_delete(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_use_watch_reserve_ignore_release_delete">>),
    {inserted, _} = ebeanstalkd:put(Q, <<"1">>),
    {reserved, JobId, <<"1">>} = ebeanstalkd:reserve(Q),
    {released} = ebeanstalkd:release(Q, JobId),
    {deleted} = ebeanstalkd:delete(Q, JobId),
    ok = ebeanstalkd:close(Q).

test_reserve_timeout(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"no_msg_tube">>),
    {timed_out} = ebeanstalkd:reserve(Q, 2),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"12">>),
    {reserved, Jb1, <<"12">>} = ebeanstalkd:reserve(Q),
    ok = ebeanstalkd:close(Q).

test_bury_peek(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_bury">>),
    {inserted, Jb2} = ebeanstalkd:put(Q, <<"2">>, [{delay, 50}]),
    {found, Jb2, <<"2">>} = ebeanstalkd:peek_delayed(Q),
    {deleted} = ebeanstalkd:delete(Q, Jb2),

    {inserted, Jb1} = ebeanstalkd:put(Q, <<"1">>),
    {found, Jb1, <<"1">>} = ebeanstalkd:peek(Q, Jb1),
    {found, Jb1, <<"1">>} = ebeanstalkd:peek_ready(Q),
    {reserved, Jb1, <<"1">>} = ebeanstalkd:reserve(Q),
    {buried} = ebeanstalkd:bury(Q, Jb1),
    {found, _, <<"1">>} = ebeanstalkd:peek_buried(Q),
    {deleted} = ebeanstalkd:delete(Q, Jb1),
    ok = ebeanstalkd:close(Q).

test_touch(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_touch">>),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"12">>),
    {reserved, Jb1, <<"12">>} = ebeanstalkd:reserve(Q),
    {touched} =ebeanstalkd:touch(Q, Jb1),
    {deleted} = ebeanstalkd:delete(Q, Jb1),
    ok = ebeanstalkd:close(Q).

test_kick(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_kick">>),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"1">>),
    {inserted, Jb2} = ebeanstalkd:put(Q, <<"2">>),
    {reserved, Jb1, <<"1">>} = ebeanstalkd:reserve(Q),
    {reserved, Jb2, <<"2">>} = ebeanstalkd:reserve(Q),
    {buried} = ebeanstalkd:bury(Q, Jb1),
    {buried} = ebeanstalkd:bury(Q, Jb2),
    {kicked, 1} = ebeanstalkd:kick(Q, 1),
    {kicked} = ebeanstalkd:kick_job(Q, Jb2),
    ok = ebeanstalkd:close(Q).

test_stats_job(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_stats_job">>),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"1">>),
    {ok, List} = ebeanstalkd:stats_job(Q, Jb1),
    ok = lists:foreach(fun({K, _V}) -> true = is_binary(K) end, List),
    14 = length(List),
    ok = ebeanstalkd:close(Q).

test_stats(_Config) ->
    % use watch just to increase the coverage
    {ok, Q} = ebeanstalkd:connect([{tube, {watch, <<"test_stats">>}}]),
    {ok, List} = ebeanstalkd:stats(Q),
    ok = lists:foreach(fun({K, _V}) -> true = is_binary(K) end, List),
    ok = ebeanstalkd:close(Q).

test_stats_tube(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    {ok, List} = ebeanstalkd:stats_tube(Q, <<"default">>),
    ok = lists:foreach(fun({K, _V}) -> true = is_binary(K) end, List),
    14 = length(List),
    ok = ebeanstalkd:close(Q).

test_list(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    {watching, _} = ebeanstalkd:watch(Q, <<"tube1">>),
    {watching, _} = ebeanstalkd:watch(Q, <<"tube2">>),

    {ok, [<<"default">>, <<"tube1">>, <<"tube2">>]} = ebeanstalkd:list_tubes_watched(Q),
    {using, <<"default">>} = ebeanstalkd:list_tube_used(Q),
    {ok, [_H | _T]} = ebeanstalkd:list_tubes(Q),
    ok = ebeanstalkd:close(Q).

test_put_in_tube2(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    {ok, List} = ebeanstalkd:stats(Q),
    case ebeanstalkd_utils:lookup(<<"cmd-put-in-tube">>, List, null) of
        null ->
            ct:print("### !!! feature put_in_tube2 not available", []);
        _ ->
            ok = use_tube(Q, <<"test_put_in_tube2">>),
            {inserted, Jb1} = ebeanstalkd:put_in_tube2(Q, <<"test_put2">>, <<"test_put_PACKET1">>),
            {inserted, Jb2} = ebeanstalkd:put_in_tube2(Q, <<"test_put2">>, <<"test_put_PACKET2">>, [{pri, 0}, {delay, 0}, {ttr, 60}]),
            {deleted} = ebeanstalkd:delete(Q, Jb1),
            {deleted} = ebeanstalkd:delete(Q, Jb2),
            ok = ebeanstalkd:close(Q)
    end.

test_capabilities(_Config) ->
    {ok, Q1} = ebeanstalkd:connect(),
    {ok, List} = ebeanstalkd:stats(Q1),
    ok = ebeanstalkd:close(Q1),
    case ebeanstalkd_utils:lookup(<<"cmd-set-caps">>, List, null) of
        null ->
            ct:print("### !!! feature set_capabilities not available", []);
        _ ->
            TubeName = <<"test_capabilities">>,
            {ok, Q} = ebeanstalkd:connect([{capabilities, [?CAPS_JOBS_WITH_TUBE]}]),
            {watching, _} = ebeanstalkd:watch(Q, TubeName),
            {watching, _} = ebeanstalkd:ignore(Q, <<"default">>),
            {inserted, Jb1} = ebeanstalkd:put_in_tube2(Q, TubeName, <<"test_put_PACKET1">>),
            {reserved, Jb1, TubeName, <<"test_put_PACKET1">>} = ebeanstalkd:reserve(Q),
            {released} = ebeanstalkd:release(Q, Jb1),
            {ok} = ebeanstalkd:set_capabilities(Q, []),
            {reserved, Jb1, <<"test_put_PACKET1">>} = ebeanstalkd:reserve(Q),
            {deleted} = ebeanstalkd:delete(Q, Jb1),
            ok = ebeanstalkd:close(Q)
    end.

% internals

use_tube(Q, Name) ->
    {watching, _} = ebeanstalkd:watch(Q, Name),
    {watching, _} = ebeanstalkd:ignore(Q, <<"default">>),
    {using, _} = ebeanstalkd:use(Q, Name),
    ok.

wait_alive(P, Attempts) when Attempts > 0 ->
    case is_process_alive(P) of
        false ->
            ok;
        _ ->
            timer:sleep(1000),
            wait_alive(P, Attempts -1)
    end;
wait_alive(_P, _Attempts) ->
    timeout.

