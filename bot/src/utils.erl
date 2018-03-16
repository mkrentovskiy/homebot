-module(utils).

-export([
          time_str/0,
          md5/1,
          to_string/1
        ]).

-include_lib("wa.hrl").

time_str() ->
  {{Y, M, D}, {H,  I, S}} = erlang:localtime(),
  lists:flatten(io_lib:format("~2.10.0b-~2.10.0b-~4.10.0b_~2.10.0b-~2.10.0b-~2.10.0b", [D, M, Y, H, I, S])).

md5(S) ->
  lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)]).

to_string(V) when is_list(V)    -> V;
to_string(V) when is_binary(V)  -> unicode:characters_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V)   -> io_lib:format("~.2f",[V]);
to_string(V) when is_atom(V)    -> atom_to_list(V).
