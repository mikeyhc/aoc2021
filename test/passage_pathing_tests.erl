-module(passage_pathing_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData0 = format_data(load_test_data("12-sample-0.txt")),
    Map0 = passage_pathing:build_map(TestData0),
    ?assertEqual(10, sets:size(passage_pathing:valid_paths(Map0))),
    TestData1 = format_data(load_test_data("12-sample-1.txt")),
    Map1 = passage_pathing:build_map(TestData1),
    ?assertEqual(19, sets:size(passage_pathing:valid_paths(Map1))),
    TestData2 = format_data(load_test_data("12-sample-2.txt")),
    Map2 = passage_pathing:build_map(TestData2),
    ?assertEqual(226, sets:size(passage_pathing:valid_paths(Map2))).

basic_test() ->
    TestData = format_data(load_test_data("12.txt")),
    Map = passage_pathing:build_map(TestData),
    ?assertEqual(4411, sets:size(passage_pathing:valid_paths(Map))).

advanced_sample_test() ->
    TestData0 = format_data(load_test_data("12-sample-0.txt")),
    Map0 = passage_pathing:build_map(TestData0),
    ?assertEqual(36, sets:size(passage_pathing:valid_paths_advanced(Map0))),
    TestData1 = format_data(load_test_data("12-sample-1.txt")),
    Map1 = passage_pathing:build_map(TestData1),
    ?assertEqual(103, sets:size(passage_pathing:valid_paths_advanced(Map1))),
    TestData2 = format_data(load_test_data("12-sample-2.txt")),
    Map2 = passage_pathing:build_map(TestData2),
    ?assertEqual(3509, sets:size(passage_pathing:valid_paths_advanced(Map2))).

advanced_test_() ->
    TestData = format_data(load_test_data("12.txt")),
    Map = passage_pathing:build_map(TestData),
    {timeout, 10,
     fun() ->
             ?assertEqual(136767,
                          sets:size(passage_pathing:valid_paths_advanced(Map)))
     end}.

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Lines = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    Fn = fun(L) ->
                 [A, B] = string:split(L, "-"),
                 {A, B}
         end,
    lists:map(Fn, Lines).
