-module(list_utils).
-export([transpose/1]).

transpose(M) ->
    transpose(M, []).

transpose([], Acc) -> lists:reverse(Acc);
transpose([[]|_], Acc) -> lists:reverse(Acc);
transpose(M, Acc) ->
    {V, Rest} = transpose_(M, [], []),
    transpose(Rest, [V|Acc]).

transpose_([], Heads, Tails) ->
    {lists:reverse(Heads), lists:reverse(Tails)};
transpose_([[H|T]|Rest], Heads, Tails) ->
    transpose_(Rest, [H|Heads], [T|Tails]).
