-module(treachery_of_whales_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = format_data(load_test_data("7-sample-0.txt")),
    ?assertEqual(37, treachery_of_whales:fuel_cost(TestData)).

basic_test() ->
    TestData = format_data(load_test_data("7.txt")),
    ?assertEqual(325528, treachery_of_whales:fuel_cost(TestData)).

advanced_sample_test() ->
    TestData = format_data(load_test_data("7-sample-0.txt")),
    ?assertEqual(168, treachery_of_whales:fuel_cost_advanced(TestData)).

advanced_test() ->
    TestData = format_data(load_test_data("7.txt")),
    ?assertEqual(85015836, treachery_of_whales:fuel_cost_advanced(TestData)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    [Line] = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    lists:map(fun list_to_integer/1, string:split(Line, ",", all)).
