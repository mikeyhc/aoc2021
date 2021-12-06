-module(lanternfish).
-export([spawn_total/2]).

spawn_total(Days, InitialFish) ->
    FishMap0 = create_fishmap(InitialFish),
    FishMap1 = run_simulation(Days, FishMap0),
    lists:sum(maps:values(FishMap1)).

create_fishmap(Initial) ->
    Inc = fun(K, M) -> maps:update_with(K, fun(V) -> V + 1 end, 1, M) end,
    lists:foldl(Inc, #{}, Initial).

run_simulation(0, Map) -> Map;
run_simulation(N, Map0) ->
    % lists:foreach(fun(V) -> io:format("~p: ~p~n", [V, maps:get(V, Map0, 0)]) end,
    %               lists:seq(0, 8)),
    % io:format("~p~n", [lists:sum(maps:values(Map0))]),
    Shift = fun(K, M) -> maps:put(K-1, maps:get(K, Map0, 0), M) end,
    Map1 = lists:foldl(Shift, Map0, lists:seq(1, 8)),
    Map2 = maps:update_with(6, fun(V) -> V + maps:get(0, Map0, 0) end, 0, Map1),
    Map3 = Map2#{8 => maps:get(0, Map0, 0)},
    run_simulation(N - 1, Map3).
