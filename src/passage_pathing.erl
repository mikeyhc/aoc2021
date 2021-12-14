-module(passage_pathing).
-export([build_map/1, valid_paths/1, valid_paths_advanced/1]).

build_map(Data) ->
    Fn = fun({K, V}, Acc0) ->
                 Acc1 = maps:update_with(K, fun(L) -> [V|L] end, [V], Acc0),
                 maps:update_with(V, fun(L) -> [K|L] end, [K], Acc1)
         end,
    lists:foldl(Fn, #{}, Data).

valid_paths(Map) ->
    S = valid_paths(Map, build_children(["start"], Map), sets:new()),
    sets_map(fun lists:reverse/1, S).

valid_paths_advanced(Map) ->
    S = valid_paths_advanced(Map, build_children(["start"], Map), sets:new()),
    sets_map(fun lists:reverse/1, S).

sets_map(Fn, S) ->
    sets:from_list(lists:map(Fn, sets:to_list(S))).

valid_paths(_Map, [], Complete) -> Complete;
valid_paths(Map, [Next|Rest], Complete) ->
    Children = build_children(Next, Map),
    Valid = fun(C) -> valid_child(C, Complete) end,
    ValidChildren = lists:filter(Valid, Children),
    HasEnd = fun([V|_]) -> V =:= "end" end,
    {Done, Doing} = lists:partition(HasEnd, ValidChildren),
    valid_paths(Map, Doing ++ Rest, sets:union(sets:from_list(Done), Complete)).

valid_paths_advanced(_Map, [], Complete) -> Complete;
valid_paths_advanced(Map, [Next|Rest], Complete) ->
    Children = build_children(Next, Map),
    Valid = fun(C) -> valid_child_advanced(C, Complete) end,
    ValidChildren = lists:filter(Valid, Children),
    HasEnd = fun([V|_]) -> V =:= "end" end,
    {Done, Doing} = lists:partition(HasEnd, ValidChildren),
    valid_paths_advanced(Map, Doing ++ Rest,
                         sets:union(sets:from_list(Done), Complete)).

build_children(Path=[Node|_], Map) ->
    lists:map(fun(C) -> [C|Path] end, maps:get(Node, Map)).

valid_child(Child, Complete) ->
    not sets:is_element(Child, Complete)
    andalso no_duplicate_small_caves(Child).

no_duplicate_small_caves(Path) ->
    SmallCaves = lists:filter(fun is_small_cave/1, Path),
    length(SmallCaves) =:= sets:size(sets:from_list(SmallCaves)).

is_small_cave([C|_]) ->
    C >= $a andalso C =< $z.

valid_child_advanced(Child, Complete) ->
    not sets:is_element(Child, Complete)
    andalso single_duplicate_small_caves(Child).

single_duplicate_small_caves(Path) ->
    Starts = length(lists:filter(fun(V) -> V =:= "start" end, Path)),
    SmallCaves = lists:filter(fun is_small_cave/1, Path),
    Grouped = group_count(SmallCaves),
    Twos = length(lists:filter(fun({_, V}) -> V =:= 2 end, Grouped)),
    Ones = length(lists:filter(fun({_, V}) -> V =:= 1 end, Grouped)),
    Starts =:= 1 andalso Twos =< 1 andalso Ones + Twos =:= length(Grouped).

group_count(List) ->
    Fn = fun(K, Acc) -> maps:update_with(K, fun(V) -> V + 1 end, 1, Acc) end,
    maps:to_list(lists:foldl(Fn, #{}, List)).
