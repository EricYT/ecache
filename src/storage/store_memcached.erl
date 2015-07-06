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
    %%mochiglobal:put(?REDIS_POOL, PoolPidList),
    ok
  catch Error:Reason ->
    io:format("--------> stack:~p~n", [erlang:get_stacktrace()]),
    {error, {Error, Reason}}
  end.

-spec stop() -> ok | {error, Reason} when
    Reason :: any().
stop() ->
  todo.

-spec get(Key) -> Value | undefined when
    Key :: any(),
    Value :: any().
get(Key) ->
  todo.

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) when is_list(Keys) ->
  todo.

-spec set(Key, Value, TTL) -> ok | {error, Reason} when
    Key :: any(),
    Value :: any(),
    TTL :: integer() | infinity,
    Reason :: any().
set(Key, Value, 'infinity') ->
  todo.

-spec sets(KeyValues) -> ok | {error, Reason} when
  KeyValues :: [{Key, Value}, ...],
  Key :: any(),
  Value :: any(),
  Reason :: any(). 
sets(KeyValues) ->
  todo.

-spec delete(Key) -> ok when
    Key :: any().
delete(Key) ->
  todo.

-spec size() -> integer().
size() ->
  todo.

-spec keys() -> Keys when
    Keys :: [term(), ...].
keys() ->
  todo.

%% Internal functions
convert_key_values([{K, V}|Tail], Acc) ->
  K_ = cache:pack_key(K),
  V_ = pack(V),
  convert_key_values(Tail, [V_, K_|Acc]);
convert_key_values([], Acc) -> lists:reverse(Acc).

-compile({inline, [do_cmd/1]}).
do_cmd(Fun) ->
  PoolList = mochiglobal:get(?REDIS_POOL),
  Pool = erlang:list_to_pid(PoolList),
  poolboy:transaction(Pool, Fun).

%% pack and unpack value
pack(Value) -> erlang:term_to_binary(Value).

unpack(Value) when is_binary(Value) -> erlang:binary_to_term(Value);
unpack(Other) -> Other.
