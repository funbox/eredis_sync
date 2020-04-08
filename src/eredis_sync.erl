%% Many parts of this code are borrowed from wooga GmbH eredis
%% library: https://github.com/wooga/eredis

-module(eredis_sync).
-export([connect/2, connect/3, connect_db/3, connect_db/4, q/2, q/3, qp/2, qp/3, close/1]).
-export_type([conn/0]).

-define(DEFAULT_TIMEOUT, 5000).
-define(NL, "\r\n").

-opaque conn() :: {term(), port()}.
-type error() :: term().
-type command() :: iodata().
-type command_list() :: [command()].
-type response() :: {ok, iodata()} | {error, error()}.
-type response_list() :: [response()].

-spec connect(inet:ip_address() | string(), inet:port_number()) -> {ok , eredis_sync:conn()} | {error, error()}.

connect(Host, Port) ->
  connect(Host, Port, ?DEFAULT_TIMEOUT).

-spec connect(inet:ip_address() | string(), inet:port_number(), timeout()) -> {ok , eredis_sync:conn()} | {error, error()}.

connect(Host, Port, Timeout) when is_binary(Host) ->
  connect(binary_to_list(Host), Port, Timeout);

connect(Host, Port, Timeout) ->
  case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}], Timeout) of
    {ok, Socket} -> {ok, {eredis_sync_parser:init(), Socket}};
    {error, _} = Error -> Error
  end.

-spec connect_db(inet:ip_address() | string(), inet:port_number(), non_neg_integer()) -> {ok, eredis_sync:conn()} | {error, error()}.

connect_db(Host, Port, Db) ->
  connect_db(Host, Port, Db, ?DEFAULT_TIMEOUT).

-spec connect_db(inet:ip_address() | string(), inet:port_number(), non_neg_integer(), timeout()) -> {ok, eredis_sync:conn()} | {error, error()}.

connect_db(Host, Port, Db, Timeout) ->
  case connect(Host, Port, Timeout) of
    {ok, Conn} ->
      case q(Conn, ["SELECT", Db]) of
        {ok, _} ->
          {ok, Conn};
        {error, _} = Error ->
          close(Conn),
          Error
      end;
    Error -> Error
  end.

-spec close(conn()) -> ok.

close({_, Socket}) ->
  ok = gen_tcp:close(Socket).

-spec q(conn(), command()) -> {ok, response_list()} | {error, error()}.

q(Conn, Command) ->
  q(Conn, Command, ?DEFAULT_TIMEOUT).

-spec q(conn(), command(), non_neg_integer()) -> {ok, response_list()} | {error, error()}.

q(Conn, Command, Timeout) ->
  case qp(Conn, [Command], Timeout) of
    {error, Error} -> {error, Error};
    [Response] -> Response
  end.

-spec qp(conn(), command_list()) -> [{ok, response_list()}] | {error, error()}.

qp(Conn, Commands) ->
    qp(Conn, Commands, ?DEFAULT_TIMEOUT).

-spec qp(conn(), command_list(), non_neg_integer()) -> [{ok, response_list()}] | {error, error()}.

qp({State, Socket}, Commands, Timeout) ->
  ok = gen_tcp:send(Socket, [create_multibulk(Command) || Command <- Commands]),
  case recv(State, Socket, length(Commands), [], Timeout) of
    {ok, Results, _NewState} -> Results;
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
  case eredis_sync_parser:parse(State, Data) of
    {ok, Result, NewState} -> recv(NewState, Socket, N - 1, [{ok, Result} | Results], Timeout);
    {ok, Result, Rest, NewState} -> handle_data(NewState, Socket, N - 1, [{ok, Result} | Results], Rest, Timeout);
    {error, Error, NewState} -> recv(NewState, Socket, N - 1, [{error, Error} | Results], Timeout);
    {error, Error, Rest, NewState} -> handle_data(NewState, Socket, N - 1, [{error, Error} | Results], Rest, Timeout);
    {continue, NewState} -> recv(NewState, Socket, N, Results, Timeout)
  end.

create_multibulk(Args) ->
  ArgCount = [<<$*>>, integer_to_list(length(Args)), <<?NL>>],
  ArgsBin = lists:map(fun to_bulk/1, lists:map(fun to_binary/1, Args)),

  [ArgCount, ArgsBin].

to_bulk(B) when is_binary(B) ->
  [<<$$>>, integer_to_list(iolist_size(B)), <<?NL>>, B, <<?NL>>].

to_binary(X) when is_list(X)    -> list_to_binary(X);
to_binary(X) when is_atom(X)    -> list_to_binary(atom_to_list(X));
to_binary(X) when is_binary(X)  -> X;
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_float(X)   -> throw({cannot_store_floats, X});
to_binary(X)                    -> term_to_binary(X).
