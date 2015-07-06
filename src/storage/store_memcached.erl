-module(store_memcached).

-behaviour(cache_behaviour).

-include("cache.hrl").

-export([start/0, stop/0, get/1, gets/1, set/3, sets/1, delete/1, size/0, keys/0]).
%%
%% APIs
%%
-spec start() -> ok | {error, Reason} when
    Reason :: any().
start() ->
  try
    %%TODO: priviate
    Size = cache:config(memcached_poolsize),
    Server = cache:config(memcached_server),
    Port = cache:config(memcached_port),
    io:format("--------------> start cached server:~p port:~p size:~p~n", [Server, Port, Size]),
    {ok, PoolPid} = poolboy:start_link([{worker_module, mcd}, {size, Size}], [Server, Port]),
    PoolPidList = erlang:pid_to_list(PoolPid),
    mochiglobal:put(?MEMCACHED_POOL, PoolPidList),
    ok
  catch Error:Reason ->
    io:format("--------> stack:~p~n", [erlang:get_stacktrace()]),
    {error, {Error, Reason}}
  end.

-spec stop() -> ok | {error, Reason} when
    Reason :: any().
stop() ->
  Pool = mochiglobal:get(?MEMCACHED_POOL),
  poolboy:stop(Pool).

-spec get(Key) -> Value | undefined when
    Key :: any(),
    Value :: any().
get(Key) ->
  KeyBin = cache:pack_key(Key),
  Fun = fun(Worker) -> mcd:get(Worker, KeyBin) end,
  case do_cmd(Fun) of
    {ok, Res} -> cache:unpack(Res);
    _Other -> undefined
  end.

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) when is_list(Keys) ->
  [ ?MODULE:get(Key) || Key<-Keys ].

-spec set(Key, Value, TTL) -> ok | {error, Reason} when
    Key :: any(),
    Value :: any(),
    TTL :: integer() | infinity,
    Reason :: any().
set(Key, Value, 'infinity') ->
  Key_ = cache:pack_key(Key),
  Value_ = cache:pack(Value),
  Fun = fun(Worker) -> mcd:set(Worker, Key_, Value_) end,
  case do_cmd(Fun) of
    {ok, _} -> ok;
    Other -> Other
  end;
set(Key, Value, TTL) ->
  Key_ = cache:pack_key(Key),
  Value_ = cache:pack(Value),
  Fun = fun(Worker) -> mcd:set(Worker, Key_, Value_, TTL) end,     
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
  sets1(KeyValues_).

sets1([{K, V, TTL}|Tail]) ->
  ok = set(K, V, TTL),
  sets1(Tail);
sets1([]) -> ok.

-spec delete(Key) -> ok when
    Key :: any().
delete(Key) ->
  Key_ = cache:pack_key(Key),
  Fun = fun(Worker) -> mcd:delete(Worker, Key_) end,
  case do_cmd(Fun) of
    {error, _}=Error -> Error;
    _ -> ok
  end.

-spec size() -> integer().
size() ->
  todo.

-spec keys() -> Keys when
    Keys :: [term(), ...].
keys() ->
  todo.

%% Internal functions
convert_key_values([{K, V}|Tail], Acc) ->
  convert_key_values(Tail, [{K, V, 'infinity'}|Acc]);
convert_key_values([{_, _, _}=Other|Tail], Acc) ->
  convert_key_values(Tail, [Other|Acc]);
convert_key_values([], Acc) -> lists:reverse(Acc).

-compile({inline, [do_cmd/1]}).
do_cmd(Fun) ->
  PoolList = mochiglobal:get(?MEMCACHED_POOL),
  Pool = erlang:list_to_pid(PoolList),
  poolboy:transaction(Pool, Fun).

