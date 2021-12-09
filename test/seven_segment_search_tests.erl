-module(seven_segment_search_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = format_data(load_test_data("8-sample-0.txt")),
    ?assertEqual(26, seven_segment_search:count_easy(TestData)).

basic_test() ->
    TestData = format_data(load_test_data("8.txt")),
    ?assertEqual(493, seven_segment_search:count_easy(TestData)).

advanced_sample_test() ->
    TestData = format_data(load_test_data("8-sample-0.txt")),
    ?assertEqual(61229, seven_segment_search:sum_outputs(TestData)).

advanced_test() ->
    TestData = format_data(load_test_data("8.txt")),
    ?assertEqual(1010460, seven_segment_search:sum_outputs(TestData)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Parts = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    lists:map(fun parse_line/1, Parts).

parse_line(Line) ->
    Parts = lists:filter(fun(L) -> L =/= [] end,
                         string:split(Line, " ", all)),
    {Inputs, ["|"|Ouputs]} = lists:split(10, Parts),
    {Inputs, Ouputs}.
