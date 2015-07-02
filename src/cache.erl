-module(cache).

-include("cache.hrl").

-export([]).

%% debug
-compile(export_all).

%%
%% API functions
%%
-spec start() -> ok | {error, Reason} when
    Reason :: any().
start() ->
  CacheModule = config(cache_module),
  io:format("--------------> start cache module :~p~n", [CacheModule]),
  CacheModule:start().

-spec stop() -> ok | {error, Reason} when
    Reason :: any().
stop() ->
  CacheModule = config(cache_module),
  io:format("--------------> stop cache module :~p~n", [CacheModule]),
  CacheModule:stop().

-spec set(Key, Value) -> ok | {error, Reason} when
    Key :: any(),
    Value :: any(),
    Reason :: any().
set(Key, Value) ->
  set(Key, Value, infinity).

-spec set(Key, Value, TTL) -> ok | {error, Reason} when
    Key :: any(),
    Value :: any(),
    TTL :: non_neg_integer() | 'infinity',
    Reason :: any().
set(Key, Value, TTL) ->
  CacheModule = config(cache_module),
  io:format("--------------> set cache module :~p~n", [CacheModule]),
  catch CacheModule:set(Key, Value, TTL).

-spec sets(KeyValues) -> ok | {error, Reason} when
  KeyValues :: [{Key, Value, TTL}, ...],
  Key :: any(),
  Value :: any(),
  TTL :: 'infinity' | integer(),
  Reason :: any().
sets(KeyValues) ->
  CacheModule = config(cache_module),
  io:format("--------------> sets cache module :~p~n", [CacheModule]),
  catch CacheModule:sets(KeyValues).

-spec get(Key) -> Value | undefined when
    Key :: any(),
    Value :: any().
get(Key) ->
  CacheModule = config(cache_module),
  io:format("--------------> get cache module :~p~n", [CacheModule]),
  catch CacheModule:get(Key).

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) ->
  CacheModule = config(cache_module),
  io:format("--------------> gets cache module :~p~n", [CacheModule]),
  catch CacheModule:gets(Keys).

-spec delete(Key) -> ok when
  Key :: any().
delete(Key) ->
  CacheModule = config(cache_module),
  io:format("--------------> erase cache module :~p~n", [CacheModule]),
  catch CacheModule:delete(Key).

-spec key(Key) -> NewKey when
    Key :: any(),
    NewKey :: binary().
key(Key) ->
  KeyBin = text:bin(Key),
  <<?KEY_PREFIX, KeyBin/binary>>.

-spec size() -> integer().
size() ->
  CacheModule = config(cache_module),
  catch CacheModule:size().


%%
%% Internal functions
%%
config(Key) -> config(cache, Key).
config(App, Key) -> env:get(App, Key).
config(App, Key, Default) -> env:get(App, Key, Default).

convert_key_values([{_, _, _}=KV|Tail], Acc) ->
  convert_key_values(Tail, [KV|Acc]);
convert_key_values([{K, V}|Tail], Acc) ->
  convert_key_values(Tail, [{K, V, 'infinity'}|Acc]);
convert_key_values([], Acc) -> lists:reverse(Acc).
