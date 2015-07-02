-module(store_redis).

-behaviour(cache_behaviour).

-include("cache.hrl").

-export([start/0, stop/0, get/1, gets/1, set/3, sets/1, delete/1, size/0]).
%%
%% APIs
%%
-spec start() -> ok | {error, Reason} when
    Reason :: any().
start() ->
  try
    %%TODO: priviate
    Size = cache:config(redis_poolsize),
    Server = cache:config(redis_server),
    Port = cache:config(redis_port),
    io:format("--------------> start redis server:~p port:~p size:~p~n", [Server, Port, Size]),
    {ok, PoolPid} = poolboy:start_link([{worker_module, eredis}, {size, Size}], [Server, Port]),
    PoolPidList = erlang:pid_to_list(PoolPid),
    mochiglobal:put(?REDIS_POOL, PoolPidList),
    ok
  catch Error:Reason ->
    io:format("--------> stack:~p~n", [erlang:get_stacktrace()]),
    {error, {Error, Reason}}
  end.

-spec stop() -> ok | {error, Reason} when
    Reason :: any().
stop() ->
  Pool = mochiglobal:get(?REDIS_POOL),
  poolboy:stop(Pool).

-spec get(Key) -> Value | undefined when
    Key :: any(),
    Value :: any().
get(Key) ->
  KeyBin = cache:key(Key),
  Fun = fun(Worker) -> eredis:q(Worker, ["GET", KeyBin]) end,
  case do_cmd(Fun) of
    {ok, Res} -> unpack(Res);
    Other ->
      undefined
  end.

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) when is_list(Keys) ->
  Keys_ = [cache:key(Key) || Key<-Keys],
  Fun = fun(Worker) -> eredis:q(Worker, ["MGET" | Keys_]) end,
  case do_cmd(Fun) of
    {ok, Res} -> [unpack(Value) || Value<-Res];
    Other ->
      undefined
  end.

-spec set(Key, Value, TTL) -> ok | {error, Reason} when
    Key :: any(),
    Value :: any(),
    TTL :: integer() | infinity,
    Reason :: any().
set(Key, Value, 'infinity') ->
  KeyBin = cache:key(Key),
  Cmd = ["SET", KeyBin, pack(Value)],
  Fun = fun(Worker) -> eredis:q(Worker, Cmd) end,
  case do_cmd(Fun) of
    {ok, _} -> ok;
    Other -> Other
  end;
set(Key, Value, TTL) ->
  KeyBin = cache:key(Key),
  Cmd = [["SET", KeyBin, pack(Value)],
         ["EXPIRE", KeyBin, TTL]],
  Fun = fun(Worker) -> eredis:qp(Worker, Cmd) end,     
  case do_cmd(Fun) of
    {error, Error}=Error -> Error;
    _ -> ok
  end.

-spec sets(KeyValues) -> ok | {error, Reason} when
  KeyValues :: [{Key, Value}, ...],
  Key :: any(),
  Value :: any(),
  Reason :: any(). 
sets(KeyValues) ->
  KeyValues_ = convert_key_values(KeyValues, []),
  Cmd = ["MSET" | KeyValues_],
  io:format("------> cmd:~p~n", [{KeyValues, Cmd}]),
  Fun = fun(Worker) -> eredis:q(Worker, Cmd) end,
  case do_cmd(Fun) of
    {error, _}=Error -> Error;
    _ -> ok
  end.

-spec delete(Key) -> ok when
    Key :: any().
delete(Key) ->
  KeyBin = cache:key(Key),
  Cmd = ["DEL", KeyBin],
  Fun = fun(Worker) -> eredis:q(Worker, Cmd) end,
  case do_cmd(Fun) of
    {error, _}=Error -> Error;
    _ -> ok
  end.

-spec size() -> integer().
size() ->
  Cmd = ["DBSIZE"],
  Fun = fun(Worker) -> eredis:q(Worker, Cmd) end,
  case do_cmd(Fun) of
    {error, _}=Error -> Error;
    {ok, Res} -> erlang:binary_to_integer(Res)
  end.

%% Internal functions
convert_key_values([{K, V}|Tail], Acc) ->
  K_ = cache:key(K),
  V_ = pack(V),
  convert_key_values(Tail, [V_, K_|Acc]);
convert_key_values([], Acc) -> lists:reverse(Acc).

-compile({inline, [do_cmd/1]}).
do_cmd(Fun) ->
  PoolList = mochiglobal:get(?REDIS_POOL),
  Pool = erlang:list_to_pid(PoolList),
  poolboy:transaction(Pool, Fun).

pack(Value) -> erlang:term_to_binary(Value).

unpack(undefined) -> undefined;
unpack(Value) -> erlang:binary_to_term(Value).
