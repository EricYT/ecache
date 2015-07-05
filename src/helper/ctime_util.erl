-module(ctime_util).

-export([expire/1, now/0]).

-spec expire(Seconds) -> Expire when
    Seconds :: integer(),
    Expire :: tuple().
expire(Seconds) ->
  ExpireSeconds = ?MODULE:now()+Seconds,
  calendar:gregorian_seconds_to_datetime(ExpireSeconds).

-spec now() -> integer().
now() ->
  Datetime = calendar:local_time(),
  calendar:datetime_to_gregorian_seconds(Datetime).

