-module(store_ets).

-export([start/0, stop/0, get/1, gets/1, set/3, erase/1]).

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
    debug:info("start ets store error:~p~n", [{Error, Reason}]),
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
    [{Value, Expire}] ->
      case maybe_expire(Expire) of
        true ->
          erase(Key),
          undefined;
        false ->
          Value
      end
  end.

-spec gets(Keys) -> Values when
    Keys :: [any(), ...],
    Values :: [any(), ...].
gets(Keys) when is_list(Keys) ->
  gets(Keys, []).

gets([Key|Tail], Acc) ->
  gets(Tail, [get(Key) | Acc]);
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
    debug:info("set error:~p~n", [{Error, Reason}]),
    {error, {Error, Reason}}
  end.

-spec erase(Key) -> ok when
    Key :: any().
erase(Key) ->
  KeyBin = cache:key(Key),
  ets:delete(?ETS_NAME, KeyBin),
  ok.



%% Internal functions
judge_expire('infinity') -> 'infinity';
judge_expire(TTL) -> ctime_util:expire(TTL).

maybe_expire(Expire) ->
  Now = ctime_util:now(),
  Expire =< Now.
