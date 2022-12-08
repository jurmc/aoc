-module(day05_app).

%%-export([part1/1, part2/1]).

%%% Exported functions

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

extract_stack_line_test() ->
    ?assertEqual(["Z", "M", "P"], extract_stack_line("[Z] [M] [P]")),
    ?assertEqual(["N", "C", ""],  extract_stack_line("[N] [C]    ")),
    ?assertEqual(["", "D", ""],   extract_stack_line("    [D]    ")).

find_empty_line_idx(Lines) -> find_empty_line_idx(Lines, 0).
find_empty_line_idx([Line|RestLines], Acc) ->
    case string:is_empty(string:trim(Line)) of
        true -> Acc;
        false -> find_empty_line_idx(RestLines, Acc+1)
    end.

find_empty_line_test() ->
    FileLines = aoc_input_app:read_file_lines2("test_input_day05.txt"),
    ?assertEqual(4, find_empty_line_idx(FileLines)).

slice(ToSlice) ->
    [SliceNotReversed, RestNotReversed] = lists:foldl(fun([H|T], [Col, Rest]) -> [[H|Col], [T|Rest]] end, [[],[]], ToSlice),
    Out = [lists:reverse(SliceNotReversed), lists:reverse(RestNotReversed)],
    Out.

transpose(Lists) -> transpose(Lists, []).
transpose(Lists, Acc) ->
    [Col,RestLists] = slice(Lists),
    case RestLists of
        [[],[],[]] -> lists:reverse([Col|Acc]);
        _ -> transpose(RestLists, [Col|Acc])
    end.

create_stacks(StacksLinesLists) ->
    TransposedLists = transpose(StacksLinesLists),
    lists:map(fun(List) ->
                      lists:flatten(lists:filter(fun(C) -> C =/= [] end, List))
              end,
              TransposedLists).

create_stacks_test() ->
    ?assertEqual(["NZ", "DCM", "P"], create_stacks([[[],"D",[]],["N","C",[]],["Z","M","P"]])).

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

extract_move_line(Line) ->
    [_, What, _, From, _, Where] = string:tokens(Line, " "),
    lists:map(fun(Str) ->  {Int, _} = string:to_integer(Str), Int end, [Where, From, Where]).

extract_move_line_test() ->
    ?assertEqual([1, 2, 1], extract_move_line("move 1 from 2 to 1")).

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
    [Stacks, [[1, 2, 1], [3, 1, 3], [2, 2, 1], [1, 1, 2]]].

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

-endif.
