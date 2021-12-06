-module(lanternfish_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = load_test_data("6-sample-0.txt"),
    ?assertEqual(5934, lanternfish:spawn_total(80, format_data(TestData))).

basic_test() ->
    TestData = load_test_data("6.txt"),
    ?assertEqual(386536, lanternfish:spawn_total(80, format_data(TestData))).

advanced_sample_test() ->
    TestData = load_test_data("6-sample-0.txt"),
    ?assertEqual(26984457539,
                 lanternfish:spawn_total(256, format_data(TestData))).

advanced_test() ->
    TestData = load_test_data("6.txt"),
    ?assertEqual(1732821262171,
                 lanternfish:spawn_total(256, format_data(TestData))).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    [Line] = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    lists:map(fun list_to_integer/1, string:split(Line, ",", all)).
