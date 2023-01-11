# ebeanstalkd

[![Build Status](https://travis-ci.com/silviucpp/ebeanstalkd.svg?branch=master)](https://travis-ci.com/github/silviucpp/ebeanstalkd)
[![GitHub](https://img.shields.io/github/license/silviucpp/ebeanstalkd)](https://github.com/silviucpp/ebeanstalkd/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/ebeanstalkd)](https://hex.pm/packages/ebeanstalkd)

A high performant Erlang client for [beanstalkd][1] work queue

### Features

- Automatically detects when server is down and is trying periodically to reconnect
- Can provide notifications to another process when connection goes down/up
- Support for connection pool using erlpool
- Very big throughput in messages per second that can be sent over one single connection (achieved over 50K/second on a single connection) 
- Protection against OOM
- The library is working with binary strings but accepts also lists.

### Quick start

Add `ebeanstalkd` as a dependency to your project. The library works with `rebar3` or `hex.pm`

```erlang
{deps, [
  {ebeanstalkd, ".*", {git, "https://github.com/silviucpp/ebeanstalkd.git", "master"}},
}.
```

The API is very simple: For example the following code is creating a connection, put a job, then reserve the job, delete it and close the connection:

```erlang
{ok, Pid} = ebeanstalkd:connect().
{inserted, Id} = ebeanstalkd:put(Pid, <<"job body">>).
{reserved, Id, <<"job body">>} = ebeanstalkd:reserve(Pid).
{deleted} = ebeanstalkd:delete(Pid, Id).
ok = ebeanstalkd:close(Pid).
```

While connectiong you can specify the following options (using `ebeanstalkd:connect/1`):

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

The API when using either the connection pool or plain connection mechanism is the same. The only difference is that instead of using a pid as the first parameter you are using the name of the pool (atom): 

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

### Notes

- The `ebeanstalkd:put_in_tube2` works only with [my server fork currently][2]. This command `put_in_tube` is not supported by the official protocol. 
A lot of clients are sending a `use` command and then a `put` but this is decreasing performances. Also `ebeanstalkd` offers this functionality in this way using `ebeanstalkd:put_in_tube`  

[1]:http://kr.github.com/beanstalkd/
[2]:https://github.com/silviucpp/beanstalkd
