-module(hydrothermal_venture).
-export([overlaps/1, overlaps_advanced/1]).

overlaps(Vents) ->
    VentMap = create_ventmap(Vents, false),
    length(lists:filter(fun(X) -> X > 1 end, maps:values(VentMap))).

overlaps_advanced(Vents) ->
    VentMap = create_ventmap(Vents, true),
    length(lists:filter(fun(X) -> X > 1 end, maps:values(VentMap))).

create_ventmap(Vents, Diagonals) ->
    lists:foldl(fun(X, Acc) -> process_vent(X, Diagonals, Acc) end, #{}, Vents).

process_vent({{X, Y0}, {X, Y1}}, _, Map) ->
    Inc = fun(V) -> V + 1 end,
    lists:foldl(fun(Y, M) -> maps:update_with({X, Y}, Inc, 1, M) end,
                Map, max_seq(Y0, Y1));
process_vent({{X0, Y}, {X1, Y}}, _, Map) ->
    Inc = fun(V) -> V + 1 end,
    lists:foldl(fun(X, M) -> maps:update_with({X, Y}, Inc, 1, M) end,
                Map, max_seq(X0, X1));
process_vent({{X0, Y0}, {X1, Y1}}, true, Map) ->
    Inc = fun(V) -> V + 1 end,
    lists:foldl(fun({X, Y}, M) -> maps:update_with({X, Y}, Inc, 1, M) end,
                Map, lists:zip(max_seq(X0, X1), max_seq(Y0, Y1)));
process_vent(_, _, Map) -> Map.

max_seq(A, B) when B < A ->
    lists:seq(A, B, -1);
max_seq(A, B) ->
    lists:seq(A, B).
