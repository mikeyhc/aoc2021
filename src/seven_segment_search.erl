-module(seven_segment_search).
-export([count_easy/1, sum_outputs/1]).

count_easy(Signals) ->
    Outputs = lists:foldl(fun lists:append/2,
                          [], lists:map(fun({_, O}) -> O end, Signals)),
    Easy = lists:filter(fun(X) -> lists:member(length(X), [2, 3, 4, 7]) end,
                        Outputs),
    length(Easy).

sum_outputs(Signals) ->
    lists:sum(lists:map(fun decode_signal/1, Signals)).

decode_signal({Inputs, Output}) ->
    Map0 = lists:foldl(fun add_basics/2, #{},
                       lists:map(fun sets:from_list/1, Inputs)),
    Map1 = build_possible(Map0),
    decode_output(lists:map(fun sets:from_list/1, Output), Map1).


add_basics(X, Map) ->
    UpdateFn = fun(K, Acc) ->
                       maps:update_with(K, fun(V) -> [X|V] end, [X], Acc)
               end,
    case sets:size(X) of
        2 -> Map#{1 => X};
        3 -> Map#{7 => X};
        4 -> Map#{4 => X};
        5 -> lists:foldl(UpdateFn, Map, [2, 3, 5]);
        6 -> lists:foldl(UpdateFn, Map, [0, 6, 9]);
        7 -> Map#{8 => X}
    end.

build_possible(#{1 := One, 2 := Twos, 3 := Threes, 4 := Four, 5 := Fives,
                 6 := Sixes, 7 := Seven, 8 := Eight, 9 := Nines, 0 := Zeros}) ->
    IsSix = fun(X) -> sets:size(sets:intersection(X, One)) =:= 1 end,
    [Six] = lists:filter(IsSix, Sixes),
    [C] = sets:to_list(sets:subtract(One, Six)),
    IsThree = fun(X) -> sets:size(sets:intersection(X, One)) =:= 2 end,
    [Three] = lists:filter(IsThree, Threes),
    IsTwo = fun(X) -> sets:is_element(C, X) end,
    [Two] = lists:filter(IsTwo, lists:delete(Three, Twos)),
    IsFive = fun(X) -> not lists:member(X, [Two, Three]) end,
    [Five] = lists:filter(IsFive, Fives),
    IsNine = fun(X) -> sets:size(sets:intersection(X, Four)) =:= 4 end,
    [Nine] = lists:filter(IsNine, Nines),
    IsZero = fun(X) -> not lists:member(X, [Six, Nine]) end,
    [Zero] = lists:filter(IsZero, Zeros),
    #{One => 1, Two => 2, Three => 3, Four => 4, Five => 5,
      Six => 6, Seven => 7, Eight => 8, Nine => 9, Zero => 0}.

decode_output(Output, Map) ->
    lists:foldl(fun(X, Acc) -> maps:get(X, Map) + Acc * 10 end, 0, Output).
