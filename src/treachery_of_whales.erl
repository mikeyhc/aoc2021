-module(treachery_of_whales).
-export([fuel_cost/1, fuel_cost_advanced/1]).

fuel_cost(Positions) ->
    MinPos = lists:min(Positions),
    MaxPos = lists:max(Positions),
    lists:min(lists:map(fun(Pos) -> score_position(Pos, Positions) end,
                        lists:seq(MinPos, MaxPos))).

score_position(Pos, Positions) ->
    lists:sum(lists:map(fun(V) -> abs(V - Pos) end, Positions)).

fuel_cost_advanced(Positions) ->
    MinPos = lists:min(Positions),
    MaxPos = lists:max(Positions),
    lists:min(lists:map(fun(Pos) -> score_position_advanced(Pos, Positions) end,
                        lists:seq(MinPos, MaxPos))).

score_position_advanced(Pos, Positions) ->
    lists:sum(lists:map(fun(V) -> advanced_cost(V, Pos) end, Positions)).

advanced_cost(Current, Goal) ->
    N = abs(Current - Goal),
    (N * (N + 1)) div 2.
