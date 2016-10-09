[![Build Status](https://travis-ci.org/savonarola/eredis_sync.svg?branch=master)](https://travis-ci.org/savonarola/eredis_sync)

`eredis_sync` is a simple Redis client which uses [eredis](https://github.com/wooga/eredis) parser
to interact with Redis through a simple `gen_tcp` socket, whithout some `gen_server` wrapper or smth.

## Build

    $ make

## Tests

    $ make test

One needs to have local `redis-server` installed to run tests.

## Sample usage

```erlang

{ok, Conn} = eredis_sync:connect({127,0,0,1}, 6379),
{ok, [{ok, <<"PONG">>}], Conn1} = eredis_sync:request(Conn, [["PING"]]),

Timeout = 1000,
{ok, [
  {ok, _},
  {ok, <<"1">>},
  {ok, <<"1">>},
  {ok, [<<"1">>, <<"2">>]}
], Conn2} = eredis_sync:request(Conn1, [
  ["DEL", "S"],
  ["SADD", "S", "1"],
  ["SADD", "S", "2"],
  ["SMEMBERS", "S"]
], Timeout),

{ok, [{error, _}], Conn3} = eredis_sync:request(Conn2, [["PANG"]]),

ok = eredis_sync:close(Conn3).

```

## License

This software is licensed under [MIT License](LICENSE).
