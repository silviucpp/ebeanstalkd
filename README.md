ebeanstalkd
================

A high performant Erlang client for [beanstalkd][1] work queue

Features
-----------

- Automatically detects when server is down and is trying periodically to reconnect
- Can provide notifications to another process when connection goes down/up
- Support for connection pool using poolboy
- Very big throughput in messages per second that can be sent over one single connection (achieved over 50K/second on a single connection) 
- Protection against OOM
- The library is working with binary strings but accepts also lists.

Quick start
-----------

Getting all deps and compile:

```
rebar get-deps
rebar compile
```

The any is very simple. For example the following piece of code is creating a connection, put a job, then reserve, delete it and close the connection:

```erlang
{ok, Pid} = ebeanstalkd:connect().
{inserted, Id} = ebeanstalkd:put(Pid, <<"job body">>).
{reserved, Id, <<"job body">>} = ebeanstalkd:reserve(Pid).
{deleted} = ebeanstalkd:delete(Pid, Id).
ok = ebeanstalkd:close(Pid).
```

The connection can also accept arguments by using `ebeanstalkd:connect/1` method with a list with the following options:

- `host` - Beanstalkd server host. Default `{127,0,0,1}` example: `{host, {127,0,0,1}`
- `port` - Beanstalkd server port. Default `11300` example: `{port, 11300}`
- `timeout` - Connection timeout in milliseconds. Default `5000` example: `{timeout, 5000}`
- `tube` - Specify what tube(s) to use or watch. Example: `{tube, {watch, [<<"tube1">>, <<"tube2">>]}}` or `{tube, {use, <<"tube1">>}}` 
- `reconnect_interval` - After how many milliseconds should try again to reconnect. Default `5000` example: `{reconnect_interval, 5000}`
- `monitor` - The process pid that should return the connection up and connection down messages. Default is `undefined` example: `{monitor, self()}`

In case the `monitor` parameter is set the specified pid will receive the following messages:

- `{connection_status, {up, ConnectionPid}}` - Triggered when connection is ready to be used
- `{connection_status, {down, ConnectionPid}}` - Triggered when connection is broken

Usage of connection pool
-----------

You can specify in the `sys.config` the pool specs in the following format:

```erlang
[
    {
        ebeanstalkd,
        [
            {pools, [
                {pool_one, 
                    [{size, 10}, {max_overflow, 0}], 
                    [{host, {127,0,0,1}}, 
                     {port, 11300}, 
                     {timeout, 5000}, 
                     {tube, {use, <<"poolname">>}}]}
            ]}
        ]
    }
].
```

The API for pool is the same for plain connections but instead of using a pid as the first parameter you are using the name of the pool (atom): 

```erlang
application:ensure_all_started(ebeanstalkd).
{inserted, Id} = ebeanstalkd:put(pool_one, <<"hello">>).
{reserved, ID, <<"hello">>} = ebeanstalkd:reserve(pool_one).
{deleted} = ebeanstalkd:delete(pool_one, ID).
```

All supported commands from the protocol are exported in `ebeanstalkd` module.

Performance testing
-----------

From the `test` folder you can use the `load_test:run/3` method to run benchmarks. Make sure you define a pool called `bk_pool` in your `sys.config` or use the one from the same location.

The `load_test/run(Method, NumberOfProcs, NumberOfRequests)` can take the following values:
 
- `Method` - specify the method to use for spawning the processes: `multispawn` or `plists` (you need to add also plists to the deps).
- `NumberOfProcs` - how many processes should spawn to send requests
- `NumberOfRequests` - how many requests should send during the test.

Example:

```erl
load_test:run(multispawn, 10, 100000).
```

[1]:http://kr.github.com/beanstalkd/