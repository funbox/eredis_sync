-module(eredis_sync_SUITE).

-export([all/0]).
-export([
  connect_ok/1,
  connect_nok/1,
  close/1,
  request_simple/1,
  request_simple_error/1,
  request_many/1,
  request_many_bulk/1,
  request_many_bulk_multi/1,
  request_timeout/1
]).

all() -> [
  connect_ok,
  connect_nok,
  close,
  request_simple,
  request_simple_error,
  request_many,
  request_many_bulk,
  request_many_bulk_multi,
  request_timeout
].

connect_ok(_Config) ->
  {ok, _} = eredis_sync:connect({127,0,0,1}, 6379).

connect_nok(_Config) ->
  {error, _} = eredis_sync:connect({127,0,0,1}, 6380).

connect() ->
  {ok, Conn} = eredis_sync:connect({127,0,0,1}, 6379),
  Conn.

close(_Config) ->
  Conn = connect(),
  ok = eredis_sync:close(Conn).

request_simple(_Config) ->
  Conn = connect(),
  {ok, [{ok, <<"PONG">>}], _} = eredis_sync:request(Conn, [["PING"]]).

request_simple_error(_Config) ->
  Conn = connect(),
  {ok, [{error, _}], _} = eredis_sync:request(Conn, [["PANG"]]).

request_many(_Config) ->
  Conn = connect(),
  {ok, [
    {ok, <<"OK">>},
    {ok, <<"OK">>},
    {ok, <<"1">>},
    {ok, <<"2">>}
  ], _} = eredis_sync:request(Conn, [
    ["SET", "X", "1"],
    ["SET", "Y", "2"],
    ["GET", "X"],
    ["GET", "Y"]
  ]).

request_many_bulk(_Config) ->
  Conn = connect(),
  {ok, [
    {ok, _},
    {ok, <<"1">>},
    {ok, <<"1">>},
    {ok, [<<"1">>, <<"2">>]}
  ], _} = eredis_sync:request(Conn, [
    ["DEL", "S"],
    ["SADD", "S", "1"],
    ["SADD", "S", "2"],
    ["SMEMBERS", "S"]
  ]).

request_many_bulk_multi(_Config) ->
  Conn = connect(),
  {ok, [
    {ok, <<"OK">>},
    {ok, <<"QUEUED">>},
    {ok, <<"QUEUED">>},
    {ok, <<"QUEUED">>},
    {ok, <<"QUEUED">>},
    {ok, [_,<<"1">>,<<"1">>,[<<"1">>,<<"2">>]]}
  ], _} = eredis_sync:request(Conn, [
    ["MULTI"],
    ["DEL", "S"],
    ["SADD", "S", "1"],
    ["SADD", "S", "2"],
    ["SMEMBERS", "S"],
    ["EXEC"]
  ], 1000).

request_timeout(_Config) ->
  Conn = connect(),
  {ok, SleepScript} = file:read_file("../../../../test/sleep.lua"),
  {Time, {error, timeout}} = timer:tc(fun() ->
    eredis_sync:request(Conn, [["EVAL", SleepScript, "0", "999999"]], 10)
  end),
  true = (Time < 20000).
