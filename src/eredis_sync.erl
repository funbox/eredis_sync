-module(eredis_sync).
-export([connect/2, request/2, request/3, close/1]).
-export_type([conn/0]).

-define(DEFAULT_TIMEOUT, 5000).

-opaque conn() :: {term(), port()}.
-type error() :: term().
-type command_list() :: [iodata()].
-type response_list() :: [response()].
-type response() :: {ok, iodata()} | {error, error()}. 

-spec connect(inet:ip_address(), inet:port_number()) -> {ok , conn()} | {error, error()}.

connect(Host, Port) ->
  case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
    {ok, Socket} -> {ok, {eredis_parser:init(), Socket}};
    {error, _} = Error -> Error
  end.

-spec close(conn()) -> ok.

close({_, Socket}) ->
  ok = gen_tcp:close(Socket).

-spec request(conn(), command_list()) -> {ok, response_list(), conn()} | {error, error()}.

request(Conn, Commands) ->
  request(Conn, Commands, ?DEFAULT_TIMEOUT).

-spec request(conn(), command_list(), non_neg_integer()) -> {ok, response_list(), conn()} | {error, error()}.

request({State, Socket}, Commands, Timeout) ->
  ok = gen_tcp:send(Socket, [[C, "\r\n"] || C <- Commands]),
  case recv(State, Socket, length(Commands), [], Timeout) of
    {ok, Results, NewState} -> {ok, Results, {NewState, Socket}};
    {error, _} = Error ->
      gen_tcp:close(Socket),
      Error
  end.

recv(_State, _Socket, _N, _Results, Timeout) when Timeout =< 0 -> {error, timeout};

recv(State, _Socket, N, Results, _Timeout) when N =< 0 ->
  {ok, lists:reverse(Results), State};

recv(State, Socket, N, Results, Timeout) ->
  {Time, RecvRes} = timer:tc(fun() -> gen_tcp:recv(Socket, 0, Timeout) end),
  case RecvRes of
    {error, _} = Error -> Error;
    {ok, Data} -> handle_data(State, Socket, N, Results, Data, Timeout - Time div 1000)
  end.

handle_data(State, Socket, N, Results, Data, Timeout) ->
  case eredis_parser:parse(State, Data) of
    {ok, Result, NewState} -> recv(NewState, Socket, N - 1, [{ok, Result} | Results], Timeout);
    {ok, Result, Rest, NewState} -> handle_data(NewState, Socket, N - 1, [{ok, Result} | Results], Rest, Timeout);
    {error, Error, NewState} -> recv(NewState, Socket, N - 1, [{error, Error} | Results], Timeout);
    {error, Error, Rest, NewState} -> handle_data(NewState, Socket, N - 1, [{error, Error} | Results], Rest, Timeout);
    {continue, NewState} -> recv(NewState, Socket, N, Results, Timeout)
  end.
