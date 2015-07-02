-module(store_ets).

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
    ets:new(?ETS_NAME, [named_table, public]),
    ok
  catch Error:Reason ->
    {error, {Error, Reason}}
  end.

-spec stop() -> ok | {error, Reason} when
    Reason :: any().
stop() ->
  ets:delete(?ETS_NAME),
  ok.

-spec get(Key) -> Value | undefined when
    Key :: any(),
    Value :: any().
get(Key) ->
  KeyBin = cache:key(Key),
  case ets:lookup(?ETS_NAME, KeyBin) of
    [] -> undefined;
    [{_, {Value, Expire}}] ->
      case maybe_expire(Expire) of
        true ->
          Value;
        false ->
          ?MODULE:delete(Key),
          undefined
      end
  end.

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) when is_list(Keys) ->
  gets(Keys, []).

gets([Key|Tail], Acc) ->
  gets(Tail, [?MODULE:get(Key) | Acc]);
gets([], Acc) -> lists:reverse(Acc).

-spec set(Key, Value, TTL) -> ok | {error, Reason} when
    Key :: any(),
    Value :: any(),
    TTL :: integer() | infinity,
    Reason :: any().
set(Key, Value, TTL) ->
  try
    KeyBin = cache:key(Key),
    TTLJudge = judge_expire(TTL),
    ets:insert(?ETS_NAME, {KeyBin, {Value, TTLJudge}}),
    ok
  catch Error:Reason ->
    {error, {Error, Reason}}
  end.

-spec sets(KeyValues) -> ok | {error, Reason} when
  KeyValues :: [{Key, Value, TTL}, ...],
  Key :: any(),
  Value :: any(),
  TTL :: 'infinity' | integer(),
  Reason :: any(). 
sets(KeyValues) ->
  KeyValues_ = convert_key_values(KeyValues, []),
  [   
      try 
        ?MODULE:set(Key, Value, TTL),
        ok  
      catch Error:Reason ->
          io:format("--------> sets error:~p~n", [{Error, Reason}]),
          {error, {Error, Reason}}
      end 
  || {Key, Value, TTL}<-KeyValues_ ].

-spec delete(Key) -> ok when
    Key :: any().
delete(Key) ->
  KeyBin = cache:key(Key),
  ets:delete(?ETS_NAME, KeyBin),
  ok.

-spec size() -> integer().
size() ->
  ets:info(?ETS_NAME, size).

%% Internal functions
judge_expire('infinity') -> 'infinity';
judge_expire(TTL) -> ctime_util:expire(TTL).

maybe_expire('infinity') -> true;
maybe_expire(Expire) ->
  Now = ctime_util:now(),
  io:format("Expire:~p Now:~p res:~p~n", [Expire, Now, Expire >= Now]),
  Expire >= Now.

convert_key_values([{_, _, _}=KV|Tail], Acc) ->
  convert_key_values(Tail, [KV|Acc]);
convert_key_values([{K, V}|Tail], Acc) ->
  convert_key_values(Tail, [{K, V, 'infinity'}|Acc]);
convert_key_values([], Acc) -> lists:reverse(Acc).
