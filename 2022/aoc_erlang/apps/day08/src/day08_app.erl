-module(day08_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    EvaluatedAndCombined = solve(FileName, fun get_visible_for_edge/1, fun combine_field_for_part1/2),
    lists:foldl(fun(List, Acc) -> lists:sum(List) + Acc end, 0, EvaluatedAndCombined).

part2(FileName) ->
    EvaluatedAndCombined = solve(FileName, fun get_scenic_score_for_edge/1, fun combine_field_for_part2/2),
    lists:max(lists:flatten(EvaluatedAndCombined)).

%%% Internal functions

solve(FileName, ScoreFun, CombineFun) ->
    Input = read_input(FileName),

    RotationForth = [0, 1, 2, 3],
    RotatedInputs = [rotate(Input, Rotation) || Rotation <- RotationForth],

    EvaluatedRotated = [ScoreFun(Matrix) || Matrix <- RotatedInputs],

    RotationBack = [0, 3, 2, 1],
    EvaluatedRotatedBack = [rotate(Matrix, Rotation) || {Rotation, Matrix} <- lists:zip(RotationBack, EvaluatedRotated)],

    EvaluatedAndCombined = lists:foldl(fun(M1, M2) ->
                                               combine_matrixes(M1, M2, CombineFun)
                                       end,
                                       hd(EvaluatedRotatedBack),
                                       tl(EvaluatedRotatedBack)).

read_input(FileName) ->
    aoc_input_app:read_file_lines(FileName, [remove_line_breaks, split_lines_into_characters]).

get_visible_for_edge(Matrix) ->
    lists:map(fun(Row) -> get_visible_for_row(Row) end, Matrix).

get_visible_for_row(Row) ->
    {_, ReversedResult} = lists:foldl(fun(Item, {Max, Acc}) ->
                                              case Item > Max of
                                                  true -> {Item, [1|Acc]};
                                                  _ -> {Max, [0|Acc]}
                                              end
                                      end,
                                      {-1, []},
                                      Row),
    lists:reverse(ReversedResult).

get_scenic_score_for_edge(Matrix) ->
    [get_scenic_scores_for_row(Row) || Row <- Matrix].

get_scenic_scores_for_row(Row) ->
    lists:map(fun(Nth) ->
                      get_scenic_score_for_row(lists:nthtail(Nth, Row))
              end,
              lists:seq(0, length(Row)-1)).

get_scenic_score_for_row([H|T]) -> get_scenic_score_for_row(T, {H, 0}).

get_scenic_score_for_row([], {_ReferenceHeigth, CurrScore}) -> CurrScore;
get_scenic_score_for_row([H|T], {ReferenceHeigth, CurrScore}) ->
    %%?debugFmt("get_scenic_score_for_row: H: ~p, T: ~p, Max: ~p, CurrScore: ~p~n", [H, T, ReferenceHeigth, CurrScore]),
    case H >= ReferenceHeigth of
        true -> CurrScore + 1;
        false -> get_scenic_score_for_row(T, {ReferenceHeigth, CurrScore + 1})
    end.

combine_field_for_part1(X, Y) ->
    case X + Y > 0 of
        true -> 1;
        _ -> 0
    end.

combine_field_for_part2(X, Y) -> X * Y.

combine_matrixes(Matrix1, Matrix2, Operation) ->
    combine_matrixes(Matrix1, Matrix2, Operation, []).

combine_matrixes([], [], _Operation, Acc) -> lists:reverse(Acc);
combine_matrixes([H1|Rest1], [H2|Rest2], Operation, Acc) ->
    NewOutRow = lists:map(fun({I1, I2}) -> Operation(I1,I2) end, lists:zip(H1,H2)),
    combine_matrixes(Rest1, Rest2, Operation, [NewOutRow|Acc]).

slice(Matrix) ->
    [ReversedSlice, ReversedRestMatrix] = lists:foldl(fun([H|T], [AccRow,AccMatrix]) -> [[H|AccRow],[T|AccMatrix]] end, [[],[]], Matrix),
    [lists:reverse(ReversedSlice), lists:reverse(ReversedRestMatrix)].

rotate(Matrix, 0) -> Matrix;
rotate(Matrix, NumRotations) ->
    Rotated = rotate90(Matrix, []),
    rotate(Rotated, NumRotations-1).

rotate90(Matrix, Acc) ->
    EmptyList = lists:duplicate(length(Matrix), []),
    case EmptyList =:= Matrix of
        true -> Acc;
        _ ->
            [Slice, Rest] = slice(Matrix),
            rotate90(Rest, [Slice|Acc])
    end.

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_visible_for_row_test() ->
    ?assertEqual([1,0,0,1,0], get_visible_for_row("30373")),
    ?assertEqual([1,1,0,0,0], get_visible_for_row(lists:reverse("30373"))).

get_visible_for_edge_test() ->
    TestInput = read_input("test_input_day08.txt"),

    Expected = [[1,0,0,1,0],
                [1,1,0,0,0],
                [1,0,0,0,0],
                [1,0,1,0,1],
                [1,1,0,1,0]],

    ?assertEqual(Expected, get_visible_for_edge(TestInput)).

rotate_test() ->
    Input     = [[1,2,3],
                 [4,5,6],
                 [7,8,9]],

    Expected1 = [[3,6,9],
                 [2,5,8],
                 [1,4,7]],
    ?assertEqual(Expected1, rotate(Input, 1)),
    ?assertEqual(Input, rotate(Input, 4)),

    Expected2 = [[9,8,7],
                 [6,5,4],
                 [3,2,1]],
    ?assertEqual(Expected2, rotate(Input, 2)),

    Expected3 = [[7,4,1],
                 [8,5,2],
                 [9,6,3]],
    ?assertEqual(Expected3, rotate(Input, 3)).

combine_matrixes_test() ->
    Matrix1 = [[1,0,0],
               [1,1,0],
               [0,1,1]],
    Matrix2 = [[0,0,1],
               [0,1,1],
               [1,1,1]],
    Expected =[[1,0,1],
               [1,1,1],
               [1,1,1]],
    ?assertEqual(Expected, combine_matrixes(Matrix1, Matrix2, fun combine_field_for_part1/2)).

get_scenic_score_for_row_test() ->
    ?assertEqual([1,1,2,1,0], get_scenic_scores_for_row([2,5,5,1,2])),
    ?assertEqual([1,1,2,1,0], get_scenic_scores_for_row([3,3,5,4,9])).

get_scenic_score_for_edge_test() ->
    Expected = [[2,1,1,1,0],
                [1,1,2,1,0],
                [4,3,1,1,0],
                [1,1,2,1,0],
                [1,2,1,1,0]],
    ?assertEqual(Expected, get_scenic_score_for_edge([[3,0,3,7,3],
                                                      [2,5,5,1,2],
                                                      [6,5,3,3,2],
                                                      [3,3,5,4,9],
                                                      [3,5,3,9,0]]
                                                    )).

part1_test() ->
    ?assertEqual(21, part1("test_input_day08.txt")),
    ?assertEqual(1703, part1("input_day08.txt")).

part2_test() ->
    ?assertEqual(8, part2("test_input_day08.txt")),
    ?assertEqual(496650, part2("input_day08.txt")).

-endif.
