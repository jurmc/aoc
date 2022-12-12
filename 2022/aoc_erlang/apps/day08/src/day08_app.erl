-module(day08_app).

%%-export([part1/1, part2/1]).

%%% Exported functions

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_input(FileName) ->
    aoc_input_app:read_file_lines(FileName, [remove_line_breaks, split_lines_into_characters]).

get_visible_for_row(Row) ->
    %%"10000".
    {_, ReversedResult} = lists:foldl(fun(Item, {Max, Acc}) ->
                                              case Item > Max of
                                                  true -> {Item, [1|Acc]};
                                                  _ -> {Max, [0|Acc]}
                                              end

                                      end,
                                      {-1, []},
                                      Row),
    lists:reverse(ReversedResult).


get_visible_for_row_test() ->
    ?assertEqual([1,0,0,1,0], get_visible_for_row("30373")),
    ?assertEqual([1,1,0,0,0], get_visible_for_row(lists:reverse("30373"))).

get_visible_for_edge(Matrix) ->
    lists:map(fun(Item) -> get_visible_for_row(Item) end, Matrix).

get_visible_for_edge_test() ->
    TestInput = read_input("test_input_day08.txt"),

    Expected = [[1,0,0,1,0],
                [1,1,0,0,0],
                [1,0,0,0,0],
                [1,0,1,0,1],
                [1,1,0,1,0]],

    ?assertEqual(Expected, get_visible_for_edge(TestInput)).

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

and_matrix(Matrix1, Matrix2) ->
    and_matrix(Matrix1, Matrix2, []).

and_matrix([], [], Acc) -> lists:reverse(Acc);
and_matrix([H1|Rest1], [H2|Rest2], Acc) ->
    NewOutRow = lists:map(fun({I1, I2}) -> case I1+I2 > 0 of
                                               true -> 1;
                                               _ -> 0
                                           end
                          end,
                                           lists:zip(H1,H2)),
    and_matrix(Rest1, Rest2, [NewOutRow|Acc]).

and_matrix_test() ->
    Matrix1 = [[1,0,0],
               [1,1,0],
               [0,1,1]],
    Matrix2 = [[0,0,1],
               [0,1,1],
               [1,1,1]],
    Expected =[[1,0,1],
               [1,1,1],
               [1,1,1]],
    ?assertEqual(Expected, and_matrix(Matrix1, Matrix2)).

part1(FileName) ->
    Input0 = read_input(FileName),
    Input90 = rotate(Input0, 1),
    Input180 = rotate(Input0, 2),
    Input270 = rotate(Input0, 3),

    And0 = get_visible_for_edge(Input0),
    And90 = get_visible_for_edge(Input90),
    And180 = get_visible_for_edge(Input180),
    And270 = get_visible_for_edge(Input270),

    And90Back = rotate(And90,3),
    And180Back = rotate(And180, 2),
    And270Back = rotate(And270, 1),

    IntermediateResult = lists:foldl(fun(M1, M2) -> and_matrix(M1, M2) end, And0, [And90Back, And180Back, And270Back]),
    lists:foldl(fun(List, Acc) -> lists:sum(List) + Acc end, 0, IntermediateResult).

part1_test() ->
    ?assertEqual(21, part1("test_input_day08.txt")),

    ?assertEqual(1703, part1("input_day08.txt")).


-endif.
