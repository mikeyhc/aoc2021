-module(transparent_origami_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    {TestData, Instructions} = format_data(load_test_data("13-sample-0.txt")),
    Map = transparent_origami:build_map(TestData),
    [Instruction|_] = Instructions,
    ?assertEqual(17, transparent_origami:count_single_fold(Instruction, Map)).

basic_test() ->
    {TestData, Instructions} = format_data(load_test_data("13.txt")),
    Map = transparent_origami:build_map(TestData),
    [Instruction|_] = Instructions,
    ?assertEqual(755, transparent_origami:count_single_fold(Instruction, Map)).

advanced_sample_test() ->
    {TestData, Instructions} = format_data(load_test_data("13-sample-0.txt")),
    Image = load_test_data("13-sample-output-0.txt"),
    Map0 = transparent_origami:build_map(TestData),
    Map1 = transparent_origami:do_folds(Instructions, Map0),
    ?assertEqual(Image, map_string(Map1)).

advanced_test() ->
    {TestData, Instructions} = format_data(load_test_data("13.txt")),
    Image = load_test_data("13-output.txt"),
    Map0 = transparent_origami:build_map(TestData),
    Map1 = transparent_origami:do_folds(Instructions, Map0),
    ?assertEqual(Image, map_string(Map1)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    binary_to_list(Data).

format_data(Input) ->
    Lines = string:split(Input, "\n", all),
    IsBlank = fun(L) -> L =/= "" end,
    {MapLines, [_|Instructions]} = lists:splitwith(IsBlank, Lines),
    {lists:map(fun to_pair/1, MapLines),
     lists:map(fun parse_instruction/1,
               lists:filter(fun(L) -> L =/= "" end, Instructions))}.

to_pair(Line) ->
    [A, B] = string:split(Line, ","),
    {list_to_integer(A), list_to_integer(B)}.

parse_instruction("fold along " ++ Ins) ->
    case Ins of
        "x=" ++ V -> {x, list_to_integer(V)};
        "y=" ++ V -> {y, list_to_integer(V)}
    end.

map_string(Map) ->
    MaxX = lists:max(lists:map(fun({X, _}) -> X end, maps:keys(Map))),
    MaxY = lists:max(lists:map(fun({_, Y}) -> Y end, maps:keys(Map))),
    L = lists:foldl(fun(Y, Acc) -> map_row(Y, MaxX, Map, Acc) end,
                    [], lists:seq(0, MaxY)),
    lists:reverse(L).

map_row(Y, MaxX, Map, Acc) ->
    Fn = fun(X, A) ->
                 case maps:is_key({X, Y}, Map) of
                     true -> [$#|A];
                     false -> [$.|A]
                 end
         end,
    [$\n|lists:foldl(Fn, Acc, lists:seq(0, MaxX))].
