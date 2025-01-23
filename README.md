# ebeanstalkd

[![Build Status](https://app.travis-ci.com/silviucpp/ebeanstalkd.svg?branch=master)](https://travis-ci.com/github/silviucpp/ebeanstalkd)
[![GitHub](https://img.shields.io/github/license/silviucpp/ebeanstalkd)](https://github.com/silviucpp/ebeanstalkd/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/ebeanstalkd)](https://hex.pm/packages/ebeanstalkd)

A high performant Erlang client for [beanstalkd][1] work queue

### Features

- Automatically detects server downtime and periodically attempts reconnection.
- Offers notifications to another process when the connection goes down or is restored.
- Includes support for connection pooling via `erlpool`.
- High message throughput, exceeding 50K messages per second over a single connection.
- Protection against Out of Memory (OOM) errors.
- Operates with binary strings but also accepts lists.

### Quick start

Add `ebeanstalkd` as a dependency to your project. The library works with `rebar3` or `hex.pm`

```erlang
{deps, [
  {ebeanstalkd, ".*", {git, "https://github.com/silviucpp/ebeanstalkd.git", "master"}},
}.
```

The API is straightforward. For instance, the following code demonstrates creating a connection, adding a job, reserving the job, deleting it, and then closing the connection:

```erlang
{ok, Pid} = ebeanstalkd:connect().
{inserted, Id} = ebeanstalkd:put(Pid, <<"job body">>).
{reserved, Id, <<"job body">>} = ebeanstalkd:reserve(Pid).
{deleted} = ebeanstalkd:delete(Pid, Id).
ok = ebeanstalkd:close(Pid).
```

While connecting you can specify the following options (using `ebeanstalkd:connect/1`):

- `host` - Beanstalkd server host. Default `{127,0,0,1}` example: `{host, {127,0,0,1}`
- `port` - Beanstalkd server port. Default `11300` example: `{port, 11300}`
- `timeout` - Connection timeout in milliseconds. Default `5000` example: `{timeout, 5000}`
- `tube` - Specify what tube(s) to use or watch. Example: `{tube, {watch, [<<"tube1">>, <<"tube2">>]}}` or `{tube, {use, <<"tube1">>}}` 
- `reconnect_interval` - After how many milliseconds should try again to reconnect. Default `5000` example: `{reconnect_interval, 5000}`
- `monitor` - The process pid that should return the connection up and connection down messages. Default is `undefined` example: `{monitor, self()}`

In case the `monitor` parameter is set, the specified pid will receive the following messages:

- `{connection_status, {up, ConnectionPid}}` - Triggered when connection is ready to be used
- `{connection_status, {down, ConnectionPid}}` - Triggered when connection is broken

### Connection pool

You can specify in the `sys.config` the pool specs in the following format:

```erlang
{ebeanstalkd, [
    {pools, [
        {bk_pool,[
            {size, 50},
            {host, {127,0,0,1}},
            {port, 11300},
            {timeout, 5000},
            {tube, {use, <<"poolname">>}}
        ]}
    ]}
]}
```

The API is identical whether you use a connection pool or a plain connection mechanism. The only difference is that, instead of using a PID as the first parameter, you use the pool name (an atom): 

```erlang
application:ensure_all_started(ebeanstalkd).
{inserted, Id} = ebeanstalkd:put(pool_one, <<"hello">>).
{reserved, ID, <<"hello">>} = ebeanstalkd:reserve(pool_one).
{deleted} = ebeanstalkd:delete(pool_one, ID).
```

All supported commands from the protocol are exported in `ebeanstalkd` module.

### Performance testing

From the `benchmark` folder you can use the `load_test:run/4` method to run benchmarks. Make sure you define a pool called `bk_pool` in your `sys.config` or use the one from the same location.

The `load_test:run(Profiling, ClientsNr, ReqNr, PayloadLength)` can take the following values:
 
- `Method` - Specify if you want to profile the erlang processes during the test.
- `ClientsNr` - how many processes should spawn to send requests
- `ReqNr` - how many requests should send during the test.
- `PayloadLength` - what length will have the message payload sent to the server

Example:

```erl
load_test:run(false, 10, 100000, 5*1024).
```

You can run the benchmark by running `make bench`

### Custom features

The following features are only available when using [my server fork][2]:

- `ebeanstalkd:put_in_tube2` allows you to place a job into a specific tube in a single command (`put_in_tube`), a functionality not supported by the official protocol. Many clients issue a `use` command followed by a `put`, which reduces performance. Instead, `ebeanstalkd` provides this functionality directly via `ebeanstalkd:put_in_tube`.
- The `capabilities` configuration accepts a list with the following options:
    - `jobs_with_tube` - Configures the server to return the tube to which a job belongs. Instead of `{Tag, JobId, JobBody}`, the server will respond with `{Tag, JobId, TubeName, JobBody}`.

[1]:https://github.com/beanstalkd/beanstalkd
[2]:https://github.com/silviucpp/beanstalkd
