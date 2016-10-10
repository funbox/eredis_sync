-module(eredis_sync_SUITE).

-export([all/0]).
-export([
  connect_ok/1,
  connect_nok/1,
  connect_db_ok/1,
  connect_db_nok/1,
  close/1,
  q_simple/1,
  q_simple_error/1,
  qp_simple/1,
  qp_bulk/1,
  qp_bulk_multi/1,
  request_timeout/1
]).

all() -> [
  connect_ok,
  connect_nok,
  connect_db_ok,
  connect_db_nok,
  close,
  q_simple,
  q_simple_error,
  qp_simple,
  qp_bulk,
  qp_bulk_multi,
  request_timeout
].

connect_ok(_Config) ->
  {ok, _} = eredis_sync:connect({127,0,0,1}, 6379).

connect_nok(_Config) ->
  {error, _} = eredis_sync:connect({127,0,0,1}, 6380).

connect_db_ok(_Config) ->
  {ok, _} = eredis_sync:connect({127,0,0,1}, 6379, 0).

connect_db_nok(_Config) ->
  {error, _} = eredis_sync:connect({127,0,0,1}, 6379, 999999).

connect() ->
  {ok, Conn} = eredis_sync:connect({127,0,0,1}, 6379),
  Conn.

close(_Config) ->
  Conn = connect(),
  ok = eredis_sync:close(Conn).

q_simple(_Config) ->
  Conn = connect(),
  {ok, <<"PONG">>} = eredis_sync:q(Conn, ["PING"]),
  {ok, <<"PONG">>} = eredis_sync:q(Conn, ["PING"]).

q_simple_error(_Config) ->
  Conn = connect(),
  {error, _} = eredis_sync:q(Conn, ["PANG"]).

qp_simple(_Config) ->
  Conn = connect(),
  [
    {ok, <<"OK">>},
    {ok, <<"OK">>},
    {ok, <<"1">>},
    {ok, <<"2">>}
  ] = eredis_sync:qp(Conn, [
    ["SET", "X", "1"],
    ["SET", "Y", "2"],
    ["GET", "X"],
    ["GET", "Y"]
  ]).

qp_bulk(_Config) ->
  Conn = connect(),
  [
    {ok, _},
    {ok, <<"1">>},
    {ok, <<"1">>},
    {ok, [<<"1">>, <<"2">>]}
  ] = eredis_sync:qp(Conn, [
    ["DEL", "S"],
    ["SADD", "S", "1"],
    ["SADD", "S", "2"],
    ["SMEMBERS", "S"]
  ]).

qp_bulk_multi(_Config) ->
  Conn = connect(),
  [
    {ok, <<"OK">>},
    {ok, <<"QUEUED">>},
    {ok, <<"QUEUED">>},
    {ok, <<"QUEUED">>},
    {ok, <<"QUEUED">>},
    {ok, [_,<<"1">>,<<"1">>,[<<"1">>,<<"2">>]]}
  ] = eredis_sync:qp(Conn, [
    ["MULTI"],
    ["DEL", "S"],
    ["SADD", "S", "1"],
    ["SADD", "S", "2"],
    ["SMEMBERS", "S"],
    ["EXEC"]
  ], 1000).

request_timeout(_Config) ->
  Conn = connect(),
  %% To make redis spin for half a second we apply technique from
  %% https://medium.com/@stockholmux/simulating-a-slow-command-with-node-redis-and-lua-efadbf913cd9
  {ok, SleepScript} = file:read_file("../../../../test/sleep.lua"),
  {Time, {error, timeout}} = timer:tc(fun() ->
    eredis_sync:q(Conn, ["EVAL", SleepScript, "0", "9999999"], 100)
  end),
  true = (Time < 200000).
