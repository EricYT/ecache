-module(ctime_util).

-export([expire/1, now/0]).

-spec expire(Seconds) -> Expire when
    Seconds :: integer(),
    Expire :: integer().
expire(Seconds) ->
  {MegaSecs, Secs, MicroSecs} = os:timestamp(),
  MegaSecs*1000000 + Secs+Seconds + MicroSecs div 1000000.

-spec now() -> integer().
now() ->
  {MegaSecs, Secs, MicroSecs} = os:timestamp(),
  MegaSecs*1000000 + Secs + MicroSecs div 1000000.

