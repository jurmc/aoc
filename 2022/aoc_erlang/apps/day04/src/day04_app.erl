-module(day04_app).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% 2-4,6-8
%% 2-3,4-5
%% 5-7,7-9
%% 2-8,3-7
%% 6-6,4-6
%% 2-6,4-8
%%
%% [{{2, 4}, {6, 8}}, {{2, 3}, {4, 5}}, {{5, 7}, {7, 9}}, {{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}, {{2, 6}, {4, 8}}]

overlap({L1, U1}, {L2, U2}) when L1 >= L2, U1 =< U2 -> true;
overlap({L1, U1}, {L2, U2}) when L2 >= L1, U2 =< U1 -> true;
overlap(_R1, _R2) -> false.

partial_overlap({L1, _U1}, {L2, U2}) when L1 >= L2, L1 =< U2 -> true;
partial_overlap({_L1, U1}, {L2, U2}) when U1 >= L2, U1 =< U2 -> true;
partial_overlap({L1, U1}, {L2, _U2}) when L2 >= L1, L2 =< U1 -> true;
partial_overlap({L1, U1}, {_L2, U2}) when U2 >= L1, U2 =< U1 -> true;
partial_overlap(_R1, _R2) -> false.

filter_overlapped(List, FilterFun) ->
    %%lists:filter(fun({R1, R2}) -> overlap(R1, R2) end, List).
    lists:filter(fun({R1, R2}) -> FilterFun(R1, R2) end, List).


overlap_test() ->
    ?assertEqual(false, overlap({2, 4}, {6, 8})),
    ?assertEqual(true, overlap({2, 8}, {3, 7})),
    ?assertEqual(false, overlap({5, 7}, {7, 9})).

partial_overlap_test() ->
    ?assertEqual(false, partial_overlap({2,4},{6,8})),
    ?assertEqual(false, partial_overlap({2,3},{4,5})),
    ?assertEqual(true, partial_overlap({5,7},{7,9})),
    ?assertEqual(true, partial_overlap({2,8},{3,7})),
    ?assertEqual(true, partial_overlap({6,6},{4,6})),
    ?assertEqual(true, partial_overlap({2,6},{4,8})).


filter_overlapped_test() ->
    Input =  [{{2, 4}, {6, 8}}, {{2, 3}, {4, 5}}, {{5, 7}, {7, 9}}, {{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}, {{2, 6}, {4, 8}}],
    ?assertEqual([{{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}], filter_overlapped(Input, fun overlap/2)).

part1(FileName) ->
    Input = read_input(FileName),
    Result = length(filter_overlapped(Input, fun overlap/2)),
    ?debugFmt("Result: ~p\n", [Result]),
    Result.

part1_test() ->
    ?assertEqual(2, part1("test_input_day04.txt")),
    part1("input_day04.txt").

part2(FileName) ->
    Input = read_input(FileName),
    Result = length(filter_overlapped(Input, fun partial_overlap/2)),
    ?debugFmt("Result: ~p\n", [Result]),
    Result.

part2_test() ->
    ?assertEqual(4, part2("test_input_day04.txt")),
    part2("input_day04.txt").

process_input_line(Line) ->
    [R1, R2] = string:tokens(Line, ","),
    [L1, U1] = string:tokens(R1, "-"),
    [L2, U2] = string:tokens(R2, "-"),
    C = fun(X) -> element(1, string:to_integer(X)) end,
    {{C(L1), C(U1)}, {C(L2), C(U2)}}.

process_input_line_test() ->
    ?assertEqual({{2,4},{6,8}}, process_input_line("2-4,6-8")).

read_input(FileName) ->
    Lines = aoc_input_app:read_file_lines(FileName),
    lists:map(fun process_input_line/1, Lines).

read_input_test() ->
    ExpectedInput =  [{{2, 4}, {6, 8}}, {{2, 3}, {4, 5}}, {{5, 7}, {7, 9}}, {{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}, {{2, 6}, {4, 8}}],
    ?assertEqual(ExpectedInput, read_input("test_input_day04.txt")).

-endif.
