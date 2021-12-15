-module(transparent_origami).
-export([build_map/1, count_single_fold/2, do_folds/2]).

build_map(Points) ->
    lists:foldl(fun(K, Acc) -> Acc#{K => true} end, #{}, Points).

count_single_fold(Instruction, Map) ->
    maps:size(fold(Instruction, Map)).

do_folds(Instructions, Map) ->
    lists:foldl(fun fold/2, Map, Instructions).

fold({x, V}, Map) ->
    Fn = fun({X, _}, _, M) when X =:= V -> M;
            ({X, Y}, _, M) when X > V -> M#{{V - (X - V), Y} => true};
            (Key, _, M) -> M#{Key => true}
         end,
    maps:fold(Fn, #{}, Map);
fold({y, V}, Map) ->
    Fn = fun({_, Y}, _, M) when Y =:= V -> M;
            ({X, Y}, _, M) when Y > V -> M#{{X, V - (Y - V)} => true};
            (Key, _, M) -> M#{Key => true}
         end,
    maps:fold(Fn, #{}, Map).
