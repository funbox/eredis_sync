[![Build Status](https://travis-ci.org/savonarola/eredis_sync.svg?branch=master)](https://travis-ci.org/savonarola/eredis_sync)

`eredis_sync` is a simple Redis client which uses [eredis](https://github.com/wooga/eredis) parser
to interact with Redis through a simple `gen_tcp` socket, whithout some `gen_server` wrapper or smth.

## Build

    $ make

## Tests

    $ make test

One needs to have local `redis-server` installed to run tests.

## Usage

With `rebar`, add actual version of the library to `rebar.config`, for example

```erlang

{eredis_sync, ".*",
    {git, "https://github.com/funbox/eredis_sync.git", {ref, "v0.1.0"}}},

```

## Example

```erlang

{ok, Conn} = eredis_sync:connect({127,0,0,1}, 6379),
{ok, <<"PONG">>} = eredis_sync:q(Conn, ["PING"]),

Timeout = 1000,
[
  {ok, _},
  {ok, <<"1">>},
  {ok, <<"1">>},
  {ok, [<<"1">>, <<"2">>]}
] = eredis_sync:qp(Conn1, [
  ["DEL", "S"],
  ["SADD", "S", "1"],
  ["SADD", "S", "2"],
  ["SMEMBERS", "S"]
], Timeout),

{error, _} = eredis_sync:q(Conn2, ["PANG"]),

ok = eredis_sync:close(Conn3).
```

## License

This software is licensed under [MIT License](LICENSE).
