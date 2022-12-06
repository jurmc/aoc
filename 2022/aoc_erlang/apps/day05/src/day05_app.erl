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
    %% TODO: fake implementation
    "ZMP".

extract_stack_line_test() ->
    ?assertEqual("ZMP", extract_stack_line(" [Z] [M] [P]")).

read_input(_FileName) ->
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
