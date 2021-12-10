-module(syntax_scoring).
-export([score_corrupt/1, score_incomplete/1]).

score_corrupt(Lines) ->
    Corrupt = lists:map(fun find_corrupt/1, Lines),
    lists:sum(lists:map(fun score_char/1, Corrupt)).

score_incomplete(Lines) ->
    Processed = lists:map(fun find_incomplete/1, Lines),
    Incomplete = lists:filter(fun(X) -> X =/= false end, Processed),
    Scores = lists:sort(lists:map(fun score_string/1, Incomplete)),
    lists:nth(length(Scores) div 2 + 1, Scores).

find_corrupt(Line) ->
    find_corrupt(Line, []).

find_corrupt([], _Stack) -> false;
find_corrupt([$(|T], Stack) -> find_corrupt(T, [$(|Stack]);
find_corrupt([$[|T], Stack) -> find_corrupt(T, [$[|Stack]);
find_corrupt([${|T], Stack) -> find_corrupt(T, [${|Stack]);
find_corrupt([$<|T], Stack) -> find_corrupt(T, [$<|Stack]);
find_corrupt([$)|T], [$(|Stack]) -> find_corrupt(T, Stack);
find_corrupt([$)|_], _Stack) -> $);
find_corrupt([$]|T], [$[|Stack]) -> find_corrupt(T, Stack);
find_corrupt([$]|_], _Stack) -> $];
find_corrupt([$}|T], [${|Stack]) -> find_corrupt(T, Stack);
find_corrupt([$}|_], _Stack) -> $};
find_corrupt([$>|T], [$<|Stack]) -> find_corrupt(T, Stack);
find_corrupt([$>|_], _Stack) -> $>.

score_char($)) -> 3;
score_char($]) -> 57;
score_char($}) -> 1197;
score_char($>) -> 25137;
score_char(false) -> 0.

find_incomplete(Line) ->
    find_incomplete(Line, []).

find_incomplete([], Stack) -> Stack;
find_incomplete([$(|T], Stack) -> find_incomplete(T, [$)|Stack]);
find_incomplete([$[|T], Stack) -> find_incomplete(T, [$]|Stack]);
find_incomplete([${|T], Stack) -> find_incomplete(T, [$}|Stack]);
find_incomplete([$<|T], Stack) -> find_incomplete(T, [$>|Stack]);
find_incomplete([H|T], [H|Stack]) -> find_incomplete(T, Stack);
find_incomplete(_List, _Stack) -> false.

score_string(String) ->
    Score = fun($), Acc) -> Acc * 5 + 1;
               ($], Acc) -> Acc * 5 + 2;
               ($}, Acc) -> Acc * 5 + 3;
               ($>, Acc) -> Acc * 5 + 4
            end,
    lists:foldl(Score, 0, String).
