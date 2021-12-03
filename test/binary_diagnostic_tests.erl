-module(binary_diagnostic_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = load_test_data("3-sample-0.txt"),
    {Gamma, Epsilon} = binary_diagnostic:power(format_data(TestData)),
    ?assertEqual(198, Gamma * Epsilon).

basic_test() ->
    TestData = load_test_data("3.txt"),
    {Gamma, Epsilon} = binary_diagnostic:power(format_data(TestData)),
    ?assertEqual(2743844, Gamma * Epsilon).

advanced_sample_test() ->
    TestData = load_test_data("3-sample-0.txt"),
    {Oxygen, CO2} = binary_diagnostic:oxygen(format_data(TestData)),
    ?assertEqual(230, Oxygen * CO2).

advanced_test() ->
    TestData = load_test_data("3.txt"),
    {Oxygen, CO2} = binary_diagnostic:oxygen(format_data(TestData)),
    ?assertEqual(6677951, Oxygen * CO2).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Parts = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    Fn = fun(S) -> lists:map(fun(X) -> X - $0 end, S) end,
    lists:map(Fn, Parts).
