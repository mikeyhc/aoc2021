-module(sonar_sweep_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = load_test_data("1-sample-0.txt"),
    ?assertEqual(7, sonar_sweep:count_increase(format_data(TestData))).

basic_test() ->
    TestData = load_test_data("1.txt"),
    ?assertEqual(1553, sonar_sweep:count_increase(format_data(TestData))).

advanced_sample_test() ->
    TestData = load_test_data("1-sample-0.txt"),
    ?assertEqual(5, sonar_sweep:count_window_increase(format_data(TestData))).

advanced_test() ->
    TestData = load_test_data("1.txt"),
    ?assertEqual(1597,
                 sonar_sweep:count_window_increase(format_data(TestData))).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Parts = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    lists:map(fun list_to_integer/1, Parts).
