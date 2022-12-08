-module(day05_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    [Stacks, Moves] = read_input(FileName),
    CraneType = t9000,
    FinalStacks = apply_moves(Stacks, Moves, CraneType),
    lists:map(fun([H|_]) -> H end, FinalStacks).

part2(FileName) ->
    [Stacks, Moves] = read_input(FileName),
    CraneType = t9001,
    FinalStacks = apply_moves(Stacks, Moves, CraneType),
    lists:map(fun([H|_]) -> H end, FinalStacks).

%%% Internal functions

apply_move(Stacks, [What, From, To], CraneType) ->
    Dict1 = orddict:from_list(lists:enumerate(Stacks)),

    {InStack, _} = orddict:take(From, Dict1),
    {OutStack, _} = orddict:take(To, Dict1),

    %% Update 'From' stack
    {MovingPart, InStackUpdated} = lists:split(What, InStack),
    Dict2 = orddict:store(From, InStackUpdated, Dict1),

    MovingPartReshufled = case CraneType of
                              t9000 -> lists:reverse(MovingPart);
                              t9001 -> MovingPart
                          end,

    OutStackUpdated = MovingPartReshufled ++ OutStack,
    Dict3 = orddict:store(To, OutStackUpdated, Dict2),

    {CraneType, lists:map(fun({_Key, Val}) -> Val end, orddict:to_list(Dict3))}.

apply_moves(Stacks, Moves, CraneType) ->
    {_IgnoredCrane, FinalStacks} = lists:foldl(fun(Move, {Crane, Acc}) -> apply_move(Acc, Move, Crane) end, {CraneType, Stacks}, Moves),
    FinalStacks.

find_empty_line_idx(Lines) -> find_empty_line_idx(Lines, 0).
find_empty_line_idx([Line|RestLines], Acc) ->
    case string:is_empty(string:trim(Line)) of
        true -> Acc;
        false -> find_empty_line_idx(RestLines, Acc+1)
    end.

create_stacks(StacksLinesLists) ->
    TransposedLists = transpose(StacksLinesLists),
    lists:map(fun(List) ->
                      lists:flatten(lists:filter(fun(C) -> C =/= [] end, List))
              end,
              TransposedLists).

extract_stack_line(Line) ->
    extract_stack_line([32|Line], []).
extract_stack_line([], Acc) -> lists:reverse(Acc);
extract_stack_line(Line, Acc) ->
    {Column, Rest} = lists:split(4, Line),
    StrippedColumn = lists:foldl(fun(C, LocAcc) ->
                                         case C of
                                             NotIgnored when NotIgnored >= 65, NotIgnored =< 90 ->[NotIgnored|LocAcc];
                                             _ -> LocAcc
                                         end
                                end, [], Column),
    extract_stack_line(Rest, [StrippedColumn|Acc]).

transpose(Lists) -> transpose(Lists, []).
transpose(Lists, Acc) ->
    [Col,RestLists] = slice(Lists),
    case RestLists of
        %% TODO: first two clauses are not generict!
        [[],[],[]] -> lists:reverse([Col|Acc]);
        [[],[],[],[],[],[],[],[]] -> lists:reverse([Col|Acc]);
        _ -> transpose(RestLists, [Col|Acc])
    end.

slice(ToSlice) ->
    [SliceNotReversed, RestNotReversed] = lists:foldl(fun([H|T], [Col, Rest]) -> [[H|Col], [T|Rest]] end, [[],[]], ToSlice),
    Out = [lists:reverse(SliceNotReversed), lists:reverse(RestNotReversed)],
    Out.

extract_move_line(Line) ->
    [_, What, _, From, _, To] = string:tokens(Line, " "),
    lists:map(fun(Str) ->  {Int, _} = string:to_integer(Str), Int end, [What, From, To]).

read_input(FileName) ->
    FileLines = aoc_input_app:read_file_lines2(FileName),
    EmptyLineIdx = find_empty_line_idx(FileLines),
    {StackLines, [_IgnoredEmpty|MoveLines]} = lists:split(EmptyLineIdx-1, FileLines),

    %% TODO: here is too late to conver binearies to lists
    StacksLinesLists = lists:map(fun(Line) -> extract_stack_line(binary:bin_to_list(Line)) end, StackLines),
    Stacks = create_stacks(StacksLinesLists),

    %% TODO: here is too late to conver binearies to lists
    %% TODO: why we have [] at the end of lines? thats why before extraction we're filtering...
    LineStrings = lists:map(fun(X) -> binary:bin_to_list(X) end, MoveLines),
    FilteredMoveLines = lists:filter(fun(X) -> X =/= [] end, LineStrings),
    Moves = lists:map(fun(Line) -> extract_move_line(Line) end, FilteredMoveLines),

    %% TODO: partially fake implementation
    [Stacks, Moves].


%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

extract_stack_line_test() ->
    ?assertEqual(["Z", "M", "P"], extract_stack_line("[Z] [M] [P]")),
    ?assertEqual(["N", "C", ""],  extract_stack_line("[N] [C]    ")),
    ?assertEqual(["", "D", ""],   extract_stack_line("    [D]    ")).

extract_move_line_test() ->
    ?assertEqual([1, 2, 1], extract_move_line("move 1 from 2 to 1")).


find_empty_line_test() ->
    FileLines = aoc_input_app:read_file_lines2("test_input_day05.txt"),
    ?assertEqual(4, find_empty_line_idx(FileLines)).

create_stacks_test() ->
    ?assertEqual(["NZ", "DCM", "P"], create_stacks([[[],"D",[]],["N","C",[]],["Z","M","P"]])).

read_input_test() ->
    [Stacks, Moves] = read_input("test_input_day05.txt"),
    ExpectedStacks = [ "NZ", "DCM", "P" ],
    ExpectedMoves = [
                     [1, 2, 1],
                     [3, 1, 3],
                     [2, 2, 1],
                     [1, 1, 2]
                    ],
    ?assertEqual(ExpectedStacks, Stacks),
    ?assertEqual(ExpectedMoves, Moves).

%%
%%     [D]    
%% [N] [C]    
%% [Z] [M] [P]
%%  1   2   3 
%% 
%% move 1 from 2 to 1
%% move 3 from 1 to 3
%% move 2 from 2 to 1
%% move 1 from 1 to 2
%%

apply_move_test() ->
    ?assertEqual({t9000, [ "DNZ", "CM", "P" ]}, apply_move([ "NZ", "DCM", "P" ], [1, 2, 1], t9000)), %% this test should test moving more than one crate
    ?assertEqual({t9001, [ "DNZ", "CM", "P" ]}, apply_move([ "NZ", "DCM", "P" ], [1, 2, 1], t9001)). %% this test should test moving more than one crate

apply_moves_test() ->
    [Stacks, Moves] = read_input("test_input_day05.txt"),
    ?assertEqual(["C", "M", "ZNDP"], apply_moves(Stacks, Moves, t9000)),
    ?assertEqual(["M", "C", "DNZP"], apply_moves(Stacks, Moves, t9001)).

part1_test() ->
    TestFileName = "test_input_day05.txt",
    TestResult = part1(TestFileName),
    ?debugFmt("Result for ~p: ~p", [TestFileName, TestResult]),
    ?assertEqual("CMZ", TestResult),

    FileName = "input_day05.txt",
    Result = part1(FileName),
    ?debugFmt("Result for ~p: ~p", [FileName, Result]),
    ?assertEqual("BZLVHBWQF", Result).

part2_test() ->
    TestFileName = "test_input_day05.txt",
    TestResult = part2(TestFileName),
    ?debugFmt("Result for ~p: ~p", [TestFileName, TestResult]),
    ?assertEqual("MCD", TestResult),

    FileName = "input_day05.txt",
    Result = part2(FileName),
    ?debugFmt("Result for ~p: ~p", [FileName, Result]),
    ?assertEqual("TDGJQTZSL", Result).

-endif.
