-module(smoke_basin_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = format_data(load_test_data("9-sample-0.txt")),
    ?assertEqual(15, smoke_basin:risk_levels_sum(TestData)).

basic_test() ->
    TestData = format_data(load_test_data("9.txt")),
    ?assertEqual(558, smoke_basin:risk_levels_sum(TestData)).

advanced_sample_test() ->
    TestData = format_data(load_test_data("9-sample-0.txt")),
    ?assertEqual(1134, smoke_basin:largest_basins(TestData)).

advanced_test() ->
    TestData = format_data(load_test_data("9.txt")),
    ?assertEqual(882942, smoke_basin:largest_basins(TestData)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Parts = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    lists:map(fun(L) -> lists:map(fun(V) -> V - $0 end, L) end, Parts).
