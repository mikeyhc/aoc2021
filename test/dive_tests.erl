-module(dive_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = load_test_data("2-sample-0.txt"),
    {Horizontal, Depth} = dive:run(format_data(TestData)),
    ?assertEqual(150, Horizontal * Depth).

basic_test() ->
    TestData = load_test_data("2.txt"),
    {Horizontal, Depth} = dive:run(format_data(TestData)),
    ?assertEqual(2091984, Horizontal * Depth).

advanced_sample_test() ->
    TestData = load_test_data("2-sample-0.txt"),
    {Horizontal, Depth} = dive:run_advanced(format_data(TestData)),
    ?assertEqual(900, Horizontal * Depth).

advanced_test() ->
    TestData = load_test_data("2.txt"),
    {Horizontal, Depth} = dive:run_advanced(format_data(TestData)),
    ?assertEqual(2086261056, Horizontal * Depth).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Parts = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    Fn = fun(S) ->
                 [Dir, Amount] = string:split(S, " "),
                 {dir_atom(Dir), list_to_integer(Amount)}
         end,
    lists:map(Fn, Parts).

dir_atom("forward") -> forward;
dir_atom("up") -> up;
dir_atom("down") -> down.
