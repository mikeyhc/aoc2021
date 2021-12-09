-module(smoke_basin).
-export([risk_levels_sum/1, largest_basins/1]).

risk_levels_sum(MapLists) ->
    Map = build_map(MapLists),
    lists:sum(lists:map(fun({_, V}) -> V + 1 end, low_points(Map))).

is_lowpoint({Point, V}, Map) ->
    lists:all(fun(P) -> V < maps:get(P, Map, 9) end, children(Point)).

low_points(Map) ->
    lists:filter(fun(V) -> is_lowpoint(V, Map) end,
                 maps:to_list(Map)).

largest_basins(MapLists) ->
    Map = build_map(MapLists),
    LowPoints = low_points(Map),
    Basins = build_basins(LowPoints, Map),
    [A,B,C|_] = lists:reverse(lists:sort(lists:map(fun sets:size/1, Basins))),
    A * B * C.

build_map(Lists) ->
    build_map(Lists, 1, #{}).

build_map([], _X, Acc) -> Acc;
build_map([H|T], X, Acc0) ->
    Acc1 = build_row(H, X, 1, Acc0),
    build_map(T, X + 1, Acc1).

build_row([], _X, _Y, Acc) -> Acc;
build_row([H|T], X, Y, Acc0) ->
    Acc1 = Acc0#{{X, Y} => H},
    build_row(T, X, Y + 1, Acc1).

build_basins(LowPoints, Map) ->
    lists:map(fun(P) -> build_basin(P, Map) end, LowPoints).

build_basin({Point, _Value}, Map) ->
    build_basin(sets:from_list(children(Point)),
                sets:from_list([Point]),
                sets:from_list([Point]),
                Map).

build_basin(Current, Visited, Set0, Map) ->
    case set_pop(Current) of
        false -> Set0;
        {H, T} ->
            case sets:is_element(H, Visited) of
                true -> build_basin(T, Visited, Set0, Map);
                false ->
                    {Set1, Next} = process_value(H, T, Visited, Set0, Map),
                    build_basin(Next, sets:add_element(H, Visited), Set1, Map)
            end
    end.

process_value(Value, Rest, Visited, Set0, Map) ->
    case maps:get(Value, Map, 9) of
        9 -> {Set0, Rest};
        _ ->
            Valid = sets:subtract(sets:from_list(children(Value)), Visited),
            {sets:add_element(Value, Set0), sets:union(Rest, Valid)}
    end.


children({X, Y}) ->
    [{X-1, Y}, {X, Y-1}, {X+1, Y}, {X, Y+1}].

set_pop(S) ->
    case sets:to_list(S) of
        [] -> false;
        [H|T] -> {H, sets:from_list(T)}
    end.
