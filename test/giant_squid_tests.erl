-module(giant_squid_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    TestData = load_test_data("4-sample-0.txt"),
    {Called, Boards} = format_data(TestData),
    {Board, Values} = giant_squid:run(Boards, Called),
    ?assertEqual(4512, score_board(Board, Values)).

basic_test() ->
    TestData = load_test_data("4.txt"),
    {Called, Boards} = format_data(TestData),
    {Board, Values} = giant_squid:run(Boards, Called),
    ?assertEqual(4662, score_board(Board, Values)).

advanced_sample_test() ->
    TestData = load_test_data("4-sample-0.txt"),
    {Called, Boards} = format_data(TestData),
    {Board, Values} = giant_squid:run_last(Boards, Called),
    ?assertEqual(1924, score_board(Board, Values)).

advanced_test() ->
    TestData = load_test_data("4.txt"),
    {Called, Boards} = format_data(TestData),
    {Board, Values} = giant_squid:run_last(Boards, Called),
    ?assertEqual(12080, score_board(Board, Values)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    Data.

format_data(Input) ->
    InputString = binary_to_list(Input),
    Parts = lists:filter(fun(L) -> length(L) > 0 end,
                         string:split(InputString, "\n", all)),
    [DrawnString|Rest] = Parts,
    Drawn = lists:map(fun list_to_integer/1,
                      string:split(DrawnString, ",", all)),
    {Drawn, read_boards(Rest)}.

read_boards([]) -> [];
read_boards([A,B,C,D,E|Rest]) ->
    [lists:map(fun row_to_integer/1, [A,B,C,D,E])|read_boards(Rest)].

row_to_integer(R) ->
    lists:map(fun list_to_integer/1,
              lists:filter(fun(X) -> X =/= "" end,
                           string:split(R, " ", all))).

score_board(Board, Values) ->
    BoardSet = sets:from_list(lists:flatten(Board)),
    [Last|_] = lists:reverse(Values),
    sets:fold(fun(A, B) -> A + B end,
              0, sets:subtract(BoardSet, sets:from_list(Values))) * Last.
