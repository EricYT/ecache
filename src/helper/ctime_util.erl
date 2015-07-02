-module(ctime_util).

-export([expire/1, now/0]).

-spec expire(Seconds) -> Expire when
    Seconds :: integer(),
    Expire :: integer().
expire(Seconds) ->
  ?MODULE:now()+Seconds.

-spec now() -> integer().
now() ->
  Now = os:timestamp(),
  Datetime = calendar:now_to_datetime(Now),
  calendar:datetime_to_gregorian_seconds(Datetime).

