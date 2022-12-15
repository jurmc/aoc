
-module(day09_app).

-define(INITIAL_POS, {1, 1}).

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    HeadPositions = apply_h_moves(load_input(FileName)),
    TailPositions = apply_t_moves(HeadPositions),
    sets:size(sets:from_list(TailPositions)).

part2(_FileName) ->
    ok.

%%% Internal functions

load_input(FileName) ->
    InputRaw = aoc_input_app:read_file_lines(FileName, [remove_line_breaks, split_lines_into_words]),
    [{Dir,element(1,string:to_integer(Val))} || [Dir,Val] <- InputRaw].

apply_h_moves(Moves) ->
    {_, Result} = lists:foldl(fun({Dir, Val}, {{Hx, Hy}, Acc}) ->
                                      NewMoves = apply_h_move({Hx, Hy}, {Dir, Val}),
                                      {lists:last(NewMoves), Acc ++ NewMoves}
                              end,
                              {?INITIAL_POS, []},
                              Moves),
    Result.

apply_t_moves(Moves) ->
    {_, RevResult} = lists:foldl(fun({Hx, Hy}, {{Tx, Ty}, Acc}) ->
                                         {NextTx, NextTy} = apply_t_move({Hx, Hy}, {Tx, Ty}),
                                         {{NextTx, NextTy}, [{NextTx, NextTy}|Acc]}
                                 end,
                                 {?INITIAL_POS, []},
                                 Moves),
    lists:reverse(RevResult).

apply_h_move({X, Y}, {"R"}) -> {X+1,Y};
apply_h_move({X, Y}, {"L"}) -> {X-1,Y};
apply_h_move({X, Y}, {"U"}) -> {X,Y+1};
apply_h_move({X, Y}, {"D"}) -> {X,Y-1};
apply_h_move({X, Y}, {Direction, 1}) -> [apply_h_move({X, Y}, {Direction})];
apply_h_move({X, Y}, {Direction, Val}) ->
    {NewX,NewY} = apply_h_move({X, Y}, {Direction}),
    [{NewX,NewY}] ++ apply_h_move({NewX, NewY}, {Direction, Val-1}).

apply_t_move({Hx,Hy},{Tx,Ty}) ->
    DistX = Hx - Tx,
    DistY = Hy - Ty,

    DirectionX = case DistX =/= 0 of
                     true -> DistX div abs(DistX);
                     _  -> 0
                 end,
    DirectionY = case DistY =/= 0 of
                     true -> DistY div abs(DistY);
                     _ -> 0
                 end,

    case (abs(DistX) > 1) or (abs(DistY) > 1) of
        true -> {Tx+DirectionX,Ty+DirectionY};
        _ -> {Tx,Ty}
    end.

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

apply_h_move_test() ->
    ?assertEqual({2,1}, apply_h_move({1,1}, {"R"})),
    ?assertEqual({2,2}, apply_h_move({2,1}, {"U"})),
    ?assertEqual({1,2}, apply_h_move({2,2}, {"L"})),
    ?assertEqual({1,1}, apply_h_move({1,2}, {"D"})).

apply_h_moves_test() ->
    ?assertEqual([{2,1}], apply_h_move({1,1}, {"R", 1})),
    ?assertEqual([{2,1},{3,1},{4,1}], apply_h_move({1,1}, {"R", 3})),
    ?assertEqual([{2,2},{2,3}], apply_h_move({2,1}, {"U", 2})),
    ?assertEqual([{4,5},{3,5}], apply_h_move({5,5}, {"L", 2})),
    ?assertEqual([{5,4},{5,3}], apply_h_move({5,5}, {"D", 2})).

apply_t_move_test() ->
    ?assertEqual({1,1}, apply_t_move({2,1}, {1,1})),
    ?assertEqual({2,1}, apply_t_move({3,1}, {1,1})),
    ?assertEqual({4,1}, apply_t_move({5,1}, {3,1})),
    ?assertEqual({4,1}, apply_t_move({5,1}, {4,1})),
    ?assertEqual({1,1}, apply_t_move({2,2}, {1,1})),
    ?assertEqual({2,2}, apply_t_move({2,3}, {1,1})),
    ?assertEqual({2,2}, apply_t_move({2,3}, {1,1})),
    ?assertEqual({2,2}, apply_t_move({3,2}, {1,1})),
    ?assertEqual({2,3}, apply_t_move({2,2}, {3,4})).

part1_test() ->
    ?assertEqual(13, part1("test_input_day09.txt")),
    ?assertEqual(6026, part1("input_day09.txt")).

-endif.

