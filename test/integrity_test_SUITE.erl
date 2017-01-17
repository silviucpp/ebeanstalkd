-module(integrity_test_SUITE).
-author("silviu.caragea").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [
    {group, ebeanstalkd_group}
].

groups() -> [
    {ebeanstalkd_group, [sequence], [
        test_put,
        test_use_watch_reserve_ignore_release_delete,
        test_bury_peek,
        test_touch,
        test_kick,
        test_stats_job,
        test_stats,
        test_stats_tube,
        test_list
    ]}
].

init_per_suite(Config) ->
    application:ensure_all_started(ebeanstalkd),
    Config.

end_per_suite(_Config) ->
    ok.


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
    ok = ebeanstalkd:close(Q),
    true.

test_use_watch_reserve_ignore_release_delete(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_use_watch_reserve_ignore_release_delete">>),
    {inserted, _} = ebeanstalkd:put(Q, <<"1">>),
    {reserved, JobId, <<"1">>} = ebeanstalkd:reserve(Q),
    {released} = ebeanstalkd:release(Q, JobId),
    {deleted} = ebeanstalkd:delete(Q, JobId),
    ok = ebeanstalkd:close(Q),
    true.

test_bury_peek(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_bury">>),
    {inserted, Jb2} = ebeanstalkd:put(Q, <<"2">>, [{delay, 50}]),
    {found, Jb2, <<"2">>} = ebeanstalkd:peek_delayed(Q),
    {deleted} = ebeanstalkd:delete(Q, Jb2),

    {inserted, Jb1} = ebeanstalkd:put(Q, <<"1">>),
    {found, Jb1, <<"1">>} = ebeanstalkd:peek_ready(Q),
    {reserved, Jb1, <<"1">>} = ebeanstalkd:reserve(Q),
    {buried} = ebeanstalkd:bury(Q, Jb1),
    {found, _, <<"1">>} = ebeanstalkd:peek_buried(Q),
    {deleted} = ebeanstalkd:delete(Q, Jb1),
    ok = ebeanstalkd:close(Q),
    true.

test_touch(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_touch">>),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"12">>),
    {reserved, Jb1, <<"12">>} = ebeanstalkd:reserve(Q),
    {touched} =ebeanstalkd:touch(Q, Jb1),
    {deleted} = ebeanstalkd:delete(Q, Jb1),
    true.

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
    ok = ebeanstalkd:close(Q),
    true.

test_stats_job(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_stats_job">>),
    {inserted, Jb1} = ebeanstalkd:put(Q, <<"1">>),
    {ok, List} = ebeanstalkd:stats_job(Q, Jb1),
    ok = lists:foreach(fun({K, _V}) -> true = is_binary(K) end, List),
    14 = length(List),
    true.

test_stats(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    ok = use_tube(Q, <<"test_stats">>),
    {ok, List} = ebeanstalkd:stats(Q),
    ok = lists:foreach(fun({K, _V}) -> true = is_binary(K) end, List),
    48 = length(List),
    true.

test_stats_tube(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    {ok, List} = ebeanstalkd:stats_tube(Q, <<"default">>),
    ok = lists:foreach(fun({K, _V}) -> true = is_binary(K) end, List),
    14 = length(List),
    true.

test_list(_Config) ->
    {ok, Q} = ebeanstalkd:connect(),
    {watching, _} = ebeanstalkd:watch(Q, <<"tube1">>),
    {watching, _} = ebeanstalkd:watch(Q, <<"tube2">>),

    {ok, [<<"default">>, <<"tube1">>, <<"tube2">>]} = ebeanstalkd:list_tubes_watched(Q),
    {using, <<"default">>} = ebeanstalkd:list_tube_used(Q),
    {ok, [_H | _T]} = ebeanstalkd:list_tubes(Q),
    true.


%internals

use_tube(Q, Name) ->
    {watching, _} = ebeanstalkd:watch(Q, Name),
    {watching, _} = ebeanstalkd:ignore(Q, <<"default">>),
    {using, _} = ebeanstalkd:use(Q, Name),
    ok.



