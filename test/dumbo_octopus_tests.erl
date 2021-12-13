-module(dumbo_octopus_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = format_data(load_test_data("11-sample-0.txt")),
    ?assertEqual(1656, dumbo_octopus:flashes(TestData)).

basic_test() ->
    TestData = format_data(load_test_data("11.txt")),
    ?assertEqual(1721, dumbo_octopus:flashes(TestData)).

advanced_sample_test() ->
    TestData = format_data(load_test_data("11-sample-0.txt")),
    ?assertEqual(195, dumbo_octopus:synced_flashes(TestData)).

advanced_test() ->
    TestData = format_data(load_test_data("11.txt")),
    ?assertEqual(298, dumbo_octopus:synced_flashes(TestData)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Lines = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    lists:map(fun(L) -> lists:map(fun(X) -> X - $0 end, L) end, Lines).
