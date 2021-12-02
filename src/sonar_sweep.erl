-module(sonar_sweep).

-export([count_increase/1, count_window_increase/1]).

count_increase([H|T]) ->
    Fn = fun(V, {P, C}) when V > P -> {V, C + 1};
            (V, {_, C}) -> {V, C}
         end,
    {_, Count} = lists:foldl(Fn, {H, 0}, T),
    Count.

count_window_increase([A,B,C|T]) ->
    Fn = fun(V, {{X, Y, Z}, N}) when V + X + Y > X + Y + Z ->
                 {{V, X, Y}, N + 1};
            (V, {{X, Y, _}, N}) ->
                 {{V, X, Y}, N}
         end,
    {_, Count} = lists:foldl(Fn, {{C, B, A}, 0}, T),
    Count.
