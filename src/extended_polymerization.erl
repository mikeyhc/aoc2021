-module(extended_polymerization).
-export([cached_diff/3]).

cached_diff([H|T], Rules, Steps) ->
    RuleMap = maps:from_list(Rules),
    {_, Parts} = lists:foldl(fun(C, {L, A}) -> {C, [[L, C]|A]} end, {H, []}, T),
    Cache = lists:foldl(fun(K, Acc) -> populate({K, Steps}, RuleMap, Acc) end,
                        #{}, Parts),
    [HP|TP] = Parts,
    Merge = fun(K=[C|_], Acc) ->
                     Counts = maps:get({K, Steps}, Cache),
                     merge_sides(C, Acc, Counts)
             end,
    Combined0 = lists:foldl(Merge, maps:get({HP, Steps}, Cache), TP),
    Sorted = lists:keysort(2, maps:to_list(Combined0)),
    [{_, Lowest}|_] = Sorted,
    [{_, Highest}|_] = lists:reverse(Sorted),
    Highest - Lowest.

populate({[A, A], 0}, _Rules, Cache) ->
    Cache#{{[A, A], 0} => #{A => 2}};
populate({[A, B], 0}, _Rules, Cache) ->
    Cache#{{[A, B], 0} => #{A => 1, B => 1}};
populate(Key={S=[A, B], N}, Rules, Cache0) ->
    case maps:get(Key, Cache0, undefined) of
        undefined ->
            C = maps:get(S, Rules),
            Cache1 = populate({[A, C], N - 1}, Rules, Cache0),
            Cache2 = populate({[C, B], N - 1}, Rules, Cache1),
            Left = maps:get({[A, C], N - 1}, Cache2),
            Right = maps:get({[C, B], N - 1}, Cache2),
            Cache2#{Key => merge_sides(C, Left, Right)};
        _Value -> Cache0
    end.

merge_sides(C, Left, Right) ->
    Merged = maps:merge_with(fun(_Key, V1, V2) -> V1 + V2 end,
                             Left, Right),
    maps:update_with(C, fun(V) -> V - 1 end, Merged).
