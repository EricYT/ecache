-module(cache_behaviour).

%%-export([start/0, stop/0, get/1, gets/1, set/3, sets/1, delete/1, size/0]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [
    {start, 0},
    {stop, 0},
    {get, 1},
    {gets, 1},
    {set, 3},
    {sets, 1},
    {delete, 1},
    {size, 0},
    {keys, 0}
  ];
behaviour_info(_Other) -> undefined.

