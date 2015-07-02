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
  CacheModule:set(Key, Value, TTL).

-spec get(Key) -> Value | undefined when
    Key :: any(),
    Value :: any().
get(Key) ->
  CacheModule = config(cache_module),
  io:format("--------------> get cache module :~p~n", [CacheModule]),
  CacheModule:get(Key).

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) ->
  CacheModule = config(cache_module),
  io:format("--------------> gets cache module :~p~n", [CacheModule]),
  CacheModule:gets(Keys).

-spec erase(Key) -> ok.
erase(Key) ->
  CacheModule = config(cache_module),
  io:format("--------------> erase cache module :~p~n", [CacheModule]),
  CacheModule:erase(Key).


-spec key(Key) -> NewKey when
    Key :: any(),
    NewKey :: binary().
key(Key) ->
  KeyBin = text:bin(Key),
  <<?KEY_PREFIX, KeyBin/binary>>.

%%
%% Internal functions
%%
config(Key) -> config(cache, Key).
config(App, Key) -> env:get(App, Key).
config(App, Key, Default) -> env:get(App, Key, Default).


