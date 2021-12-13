-module(dumbo_octopus).
-export([flashes/1, synced_flashes/1]).

flashes(ListMap) ->
    Map0 = build_map(ListMap),
    {_Map1, Flashes} = run_simulation(Map0),
    Flashes.

synced_flashes(ListMap) ->
    Map = build_map(ListMap),
    find_sync_point(Map).

find_sync_point(Map) ->
    find_sync_point(Map, 1).

find_sync_point(Map0, N) ->
    {Map1, Flashes} = step_simulation(Map0),
    if Flashes =:= 100 -> N;
       true -> find_sync_point(Map1, N + 1)
    end.

run_simulation(Map) ->
    Fn = fun(_Idx, {M0, F0}) ->
                 {M1, F1} = step_simulation(M0),
                 {M1, F0 + F1}
         end,
    lists:foldl(Fn, {Map, 0}, lists:seq(1, 100)).

step_simulation(Map0) ->
    Map1 = increase_levels(Map0),
    run_flashes(Map1).

run_flashes(Map) ->
    case has_nine(Map) of
        true -> run_flashes(Map, 0);
        false -> {Map, 0}
    end.

run_flashes(Map0, Flashes0) ->
    {Map1, Flashes1} = lists:foldl(fun flash_fold_s/2, {Map0, Flashes0},
                                   lists:map(fun({K, _}) -> K end,
                                             lists:filter(fun({_, V}) -> V > 9 end,
                                                          maps:to_list(Map0)))),
    case has_nine(Map1) of
        true -> run_flashes(Map1, Flashes1);
        false -> {Map1, Flashes1}
    end.

flash_fold_s({X, Y}, {Map0, Flashes}) ->
    Fn = fun(K, M) -> maps:update_with(K, fun(V) -> V + 1 end, M) end,
    Neighbors = [{X - 1, Y - 1}, {X, Y - 1}, {X + 1, Y - 1},
                 {X - 1, Y}, {X + 1, Y},
                 {X - 1, Y + 1}, {X, Y + 1}, {X + 1, Y + 1}],
    Valid = fun(K) -> maps:is_key(K, Map0) andalso maps:get(K, Map0) =/= 0 end,
    ValidNeighbors = lists:filter(Valid, Neighbors),
    Map1 = lists:foldl(Fn, Map0, ValidNeighbors),
    Map2 = Map1#{{X, Y} => 0},
    {Map2, Flashes + 1}.

has_nine(Map) ->
    lists:any(fun(V) -> V > 9 end, maps:values(Map)).

increase_levels(Map) ->
    maps:map(fun(_, V) -> V + 1 end, Map).

build_map(ListMap) ->
    lists:foldl(fun({Y, Line}, Map) -> build_row(Y, Line, Map) end,
                #{}, lists:zip(lists:seq(1, length(ListMap)), ListMap)).

build_row(Y, Row, Map) ->
    lists:foldl(fun({X, V}, M) -> M#{{X, Y} => V} end,
                Map, lists:zip(lists:seq(1, length(Row)), Row)).
