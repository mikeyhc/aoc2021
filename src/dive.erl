-module(dive).
-export([run/1, run_advanced/1]).

run(Course) ->
    Fn = fun({forward, V}, {H, D}) -> {H + V, D};
            ({up, V}, {H, D}) -> {H, D - V};
            ({down, V}, {H, D}) -> {H, D + V}
         end,
    lists:foldl(Fn, {0, 0}, Course).

run_advanced(Course) ->
    Fn = fun({forward, V}, {H, D, A}) -> {H + V, D + V * A, A};
            ({up, V}, {H, D, A}) -> {H, D, A - V};
            ({down, V}, {H, D, A}) -> {H, D, A + V}
         end,
    {H, D, _} = lists:foldl(Fn, {0, 0, 0}, Course),
    {H, D}.
