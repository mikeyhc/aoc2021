-module(extended_polymerization_tests).

-include_lib("eunit/include/eunit.hrl").

basic_sample_test() ->
    {Start, Rules} = format_data(load_test_data("14-sample-0.txt")),
    ?assertEqual(1588, extended_polymerization:cached_diff(Start, Rules, 10)).

basic_test() ->
    {Start, Rules} = format_data(load_test_data("14.txt")),
    ?assertEqual(2360, extended_polymerization:cached_diff(Start, Rules, 10)).

advanced_sample_test() ->
    {Start, Rules} = format_data(load_test_data("14-sample-0.txt")),
    ?assertEqual(2188189693529,
                 extended_polymerization:cached_diff(Start, Rules, 40)).

advanced_test() ->
    {Start, Rules} = format_data(load_test_data("14.txt")),
    ?assertEqual(2967977072188,
                 extended_polymerization:cached_diff(Start, Rules, 40)).

load_test_data(File) ->
    Path = code:priv_dir(aoc2021) ++ "/test-data/" ++ File,
    {ok, Data} = file:read_file(Path),
    binary_to_list(Data).

format_data(Input) ->
    [Start|Rules] = lists:filter(fun(S) -> S =/= "" end,
                                 string:split(Input, "\n", all)),
    {Start, lists:map(fun to_rule/1, Rules)}.

to_rule(Line) ->
    [A, "->", [B]] = string:split(Line, " ", all),
    {A, B}.
