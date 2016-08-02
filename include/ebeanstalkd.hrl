-author("silviu.caragea").

-define(PRINT_MSG(Format, Args),
    io:format(Format, Args)).

-define(DEBUG_MSG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).

%commands

-define(BK_PUT(Data, Pri, Delay, Ttr, Size), {[<<"put">>, Pri, Delay, Ttr, Size], Data}).
-define(BK_USE(Tube), [<<"use">>, Tube]).
-define(BK_RESERVE(), <<"reserve">>).
-define(BK_RESERVE(Timeout), [<<"reserve-with-timeout">>, Timeout]).
-define(BK_DELETE(ID), [<<"delete">>, ID]).
-define(BK_RELEASE(ID, Pri, Delay), [<<"release">>, ID, Pri, Delay]).
-define(BK_BURY(ID, Priority), [<<"bury">>, ID, Priority]).
-define(BK_TOUCH(ID), [<<"touch">>, ID]).
-define(BK_WATCH(Tube), [<<"watch">>, Tube]).
-define(BK_IGNORE(Tube), [<<"ignore">>, Tube]).
-define(BK_PEEK(ID), [<<"peek">>, ID]).
-define(BK_PEEK_READY(), <<"peek-ready">>).
-define(BK_PEEK_DELAYED(), <<"peek-delayed">>).
-define(BK_PEEK_BURIED(), <<"peek-buried">>).
-define(BK_KICK(Bound), [<<"kick">>, Bound]).
-define(BK_KICK_JOB(ID), [<<"kick-job">>, ID]).
-define(BK_STATS_JOB(ID), [<<"stats-job">>, ID]).
-define(BK_STATS_TUBE(Tube), [<<"stats-tube">>, Tube]).
-define(BK_STATS(), <<"stats">>).
-define(BK_LIST_TUBES(), <<"list-tubes">>).
-define(BK_LIST_TUBE_USED(), <<"list-tube-used">>).
-define(BK_LIST_TUBES_WATCHED(), <<"list-tubes-watched">>).

%defaults

-define(DEFAULT_TUBE_NAME, <<"default">>).
-define(MAX_PENDING_REQUESTS_QUEUE, 100).

-define(DEFAULT_IP, {127, 0, 0, 1}).
-define(DEFAULT_PORT, 11300).
-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_TUBE, undefined).

-define(RECONNECT_MS, 5000).
-define(DEFAULT_PRIORITY, 0).
-define(DEFAULT_DELAY, 0).
-define(DEFAULT_TTR, 60).