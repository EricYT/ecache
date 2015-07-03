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
  CacheModule:start().

-spec stop() -> ok | {error, Reason} when
    Reason :: any().
stop() ->
  CacheModule = config(cache_module),
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
  catch CacheModule:set(Key, Value, TTL).

-spec sets(KeyValues) -> ok | {error, Reason} when
  KeyValues :: [{Key, Value, TTL}, ...],
  Key :: any(),
  Value :: any(),
  TTL :: 'infinity' | integer(),
  Reason :: any().
sets(KeyValues) ->
  CacheModule = config(cache_module),
  catch CacheModule:sets(KeyValues).

-spec get(Key) -> Value | undefined when
    Key :: any(),
    Value :: any().
get(Key) ->
  CacheModule = config(cache_module),
  catch CacheModule:get(Key).

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) ->
  CacheModule = config(cache_module),
  catch CacheModule:gets(Keys).

-spec delete(Key) -> ok when
  Key :: any().
delete(Key) ->
  CacheModule = config(cache_module),
  catch CacheModule:delete(Key).

-spec size() -> integer().
size() ->
  CacheModule = config(cache_module),
  catch CacheModule:size().

-spec keys() -> [term(), ...].
keys() ->
  CacheModule = config(cache_module),
  catch CacheModule:keys().

-spec pack_key(Key) -> NewKey when
    Key :: term(),
    NewKey :: binary().
pack_key(Key) ->
  KeyBin = erlang:term_to_binary(Key),
  << ?KEY_PREFIX, KeyBin/binary >>.

-spec unpack_key(KeyBin) -> Key when
    KeyBin :: binary(),
    Key :: term().
unpack_key(<< ?KEY_PREFIX, KeyBin/binary >>) ->
  erlang:binary_to_term(KeyBin).


%%
%% Internal functions
%%
config(Key) -> config(cache, Key).
config(App, Key) -> env:get(App, Key).
config(App, Key, Default) -> env:get(App, Key, Default).

