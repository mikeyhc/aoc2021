-module(syntax_scoring_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = format_data(load_test_data("10-sample-0.txt")),
    ?assertEqual(26397, syntax_scoring:score_corrupt(TestData)).

basic_test() ->
    TestData = format_data(load_test_data("10.txt")),
    ?assertEqual(166191, syntax_scoring:score_corrupt(TestData)).

advanced_sample_test() ->
    TestData = format_data(load_test_data("10-sample-0.txt")),
    ?assertEqual(288957, syntax_scoring:score_incomplete(TestData)).

advanced_test() ->
    TestData = format_data(load_test_data("10.txt")),
    ?assertEqual(1152088313, syntax_scoring:score_incomplete(TestData)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    lists:filter(fun(L) -> length(L) > 0 end,
                 string:split(InputString, "\n", all)).
