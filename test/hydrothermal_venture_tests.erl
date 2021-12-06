-module(hydrothermal_venture_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = load_test_data("5-sample-0.txt"),
    ?assertEqual(5, hydrothermal_venture:overlaps(format_data(TestData))).

basic_test() ->
    TestData = load_test_data("5.txt"),
    ?assertEqual(7644, hydrothermal_venture:overlaps(format_data(TestData))).

advanced_sample_test() ->
    TestData = load_test_data("5-sample-0.txt"),
    Count = hydrothermal_venture:overlaps_advanced(format_data(TestData)),
    ?assertEqual(12, Count).

advanced_test() ->
    TestData = load_test_data("5.txt"),
    Count = hydrothermal_venture:overlaps_advanced(format_data(TestData)),
    ?assertEqual(18627, Count).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Parts = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    lists:map(fun process_line/1, Parts).

process_line(Line) ->
    [A, B] = string:split(Line, " -> "),
    [C, D] = string:split(A, ","),
    [E, F] = string:split(B, ","),
    {{list_to_integer(C), list_to_integer(D)},
     {list_to_integer(E), list_to_integer(F)}}.
