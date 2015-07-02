-module(text).

-export([bin/1]).

-spec bin(In) -> Out when
    In :: atom() | integer() | float() | tuple() | string() | binary(),
    Out :: binary().
bin(In) when is_atom(In) ->
  bin(atom_to_list(In));
bin(In) when is_integer(In) ->
  bin(integer_to_list(In));
bin(In) when is_float(In) ->
  bin(float_to_list(In));
bin(In) when is_tuple(In) ->
  bin(tuple_to_list(In));
bin(In) when is_list(In) ->
  list_to_binary(In);
bin(In) when is_binary(In) -> In.
