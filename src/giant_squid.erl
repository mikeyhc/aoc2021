-module(giant_squid).
-export([run/2, run_last/2]).

run(BoardsLists, Called) ->
    Boards = lists:map(fun list_to_board/1, BoardsLists),
    run_simulation(Boards, Called).

run_last(BoardsLists, Called) ->
    Boards = lists:map(fun list_to_board/1, BoardsLists),
    run_last_simulation(Boards, Called).

run_simulation(Boards, Called) ->
    run_simulation(Boards, Called, []).

run_last_simulation(Boards, Called) ->
    run_last_simulation(Boards, Called, []).

run_simulation(Boards0, [Move|Rest], Moves) ->
    Boards1 = apply_move(Move, Boards0),
    case winning_board(Boards1) of
        false ->
            run_simulation(Boards1, Rest, [Move|Moves]);
        Winner ->
            {Winner, lists:reverse([Move|Moves])}
    end.

run_last_simulation(Boards0, [Move|Rest], Moves) ->
    Boards1 = apply_move(Move, Boards0),
    case lists:filter(fun losing_board/1, Boards1) of
        [] ->
            [{_Rows, _Cols, Winner}] = Boards0,
            {Winner, lists:reverse([Move|Moves])};
        Boards2 ->
            run_last_simulation(Boards2, Rest, [Move|Moves])
    end.

list_to_board(ListBoard) ->
    {lists:map(fun sets:from_list/1, ListBoard),
     lists:map(fun sets:from_list/1, list_utils:transpose(ListBoard)),
     ListBoard}.

apply_move(Move, Boards) ->
    lists:map(fun(Board) -> apply_move_(Move, Board) end, Boards).

apply_move_(Move, {Rows, Cols, Orig}) ->
    Remove = fun(B) -> sets:del_element(Move, B) end,
    {lists:map(Remove, Rows),
     lists:map(Remove, Cols),
     Orig}.

winning_board(Boards) ->
    IsZero = fun(S) -> sets:size(S) =:= 0 end,
    Fn = fun({Rows, Cols, _Board}) ->
                 lists:any(IsZero, Rows) orelse lists:any(IsZero, Cols)
         end,
    case lists:search(Fn, Boards) of
        {value, {_Rows, _Cols, Board}} -> Board;
        false -> false
    end.

losing_board({Rows, Cols, _Orig}) ->
    NotZero = fun(S) -> sets:size(S) =/= 0 end,
    lists:all(NotZero, Rows) andalso lists:all(NotZero, Cols).
