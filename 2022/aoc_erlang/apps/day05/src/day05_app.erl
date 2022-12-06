-module(day05_app).

%%-export([part1/1, part2/1]).

%%% Exported functions

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

create_stacks(StacksLinesLists) ->
    not_implemented.

create_stacks_test() ->
    ?assertEqual(["NZ", "DCM", "P"], create_stacks([[[],"D",[]],["N","C",[]],["Z","M","P"]])).

read_input(FileName) ->
    FileLines = aoc_input_app:read_file_lines2(FileName),
    EmptyLineIdx = find_empty_line_idx(FileLines),
    {StackLines, [_IgnoredEmpty|MoveLines]} = lists:split(EmptyLineIdx-1, FileLines),

    StacksLinesLists = lists:map(fun(Line) -> extract_stack_line(binary:bin_to_list(Line)) end, StackLines),
    Stacks = create_stacks(StacksLinesLists),
    ?debugFmt("Stacks: ~p\n", [Stacks]),

    %% TODO: fake implementation
    [["NZ", "DCM", "P"], [[1, 2, 1], [3, 1, 3], [2, 2, 1], [1, 1, 2]]].

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
