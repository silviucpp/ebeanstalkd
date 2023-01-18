% logs

-include_lib("kernel/include/logger.hrl").

-define(LOG_PRINT(Format, Args),
    io:format(Format, Args)).

% stacktrace

-ifdef(OTP_RELEASE). %% this implies 21 or higher
    -define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
    -define(GET_STACK(Stacktrace), Stacktrace).
-else.
    -define(EXCEPTION(Class, Reason, _), Class:Reason).
    -define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

% commands

-define(BK_PUT(Data, Pri, Delay, Ttr, Size), {[<<"put">>, Pri, Delay, Ttr, Size], Data}).
-define(BK_PUT_IN_TUBE(Tube, Data, Pri, Delay, Ttr, Size), {[<<"put-in-tube">>, Tube, Pri, Delay, Ttr, Size], Data}).
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

% defaults

-define(DEFAULT_TUBE_NAME, <<"default">>).
-define(MAX_PENDING_REQUESTS_QUEUE, 1000).
-define(DEFAULT_WAIT_RESPONSE_TIMEOUT_MS, 10000).

-define(DEFAULT_IP, {127, 0, 0, 1}).
-define(DEFAULT_PORT, 11300).
-define(DEFAULT_CONNECTION_TIMEOUT_MS, 5000).
-define(DEFAULT_TUBE, undefined).

-define(RECONNECT_MS, 5000).
-define(DEFAULT_PRIORITY, 0).
-define(DEFAULT_DELAY, 0).
-define(DEFAULT_TTR, 120).

% protocol end line and whitespace

-define(BIN_END_LINE, <<"\r\n">>).
-define(STR_END_LINE, "\r\n").
-define(STR_WHITE_SPACE, " ").
-define(BIN_WHITE_SPACE, <<" ">>).

% types

-type reason() :: term().
-type host() :: inet:ip_address() | inet:hostname().
-type tube() :: binary().
-type watch_tube() :: {watch, [tube()]}.
-type use_tube() :: {use, tube()}.
-type con_ref() :: atom() | pid().
-type job_id() :: integer().

- type pool_option() :: erlpool:pool_option() | connect_option().

-type connect_option() ::
    {host, host()} |
    {port, non_neg_integer()} |
    {timeout, timeout()} |
    {tube, watch_tube() | use_tube() | undefined} |
    {reconnect_interval, non_neg_integer()} |
    {monitor, pid() | undefined}.

-type put_option() ::
    {pri, non_neg_integer()} |
    {delay, non_neg_integer()} |
    {ttr, non_neg_integer()}.

-type release_option() ::
    {pri, non_neg_integer()} |
    {delay, non_neg_integer()}.
