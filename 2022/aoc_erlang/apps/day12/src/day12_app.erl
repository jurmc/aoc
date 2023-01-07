-module(day12_app).

%%-export([part1/1, part2/1]).
-compile(export_all).

%%% Exported functions

part2(_FileName) ->
    ok.

part1(_FileName) ->
    ok.

%%% Internal functions

load_input(FileName) ->
    Lines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]),
    MaxY = length(Lines),
    MaxX = length(hd(Lines)),
    LinesWithY = [{Y, lists:nth(Y, Lines)} || Y <- lists:seq(1, MaxY)],
    CoordsList = lists:foldl(fun({Y, Line}, Acc) ->
                                     Acc ++ [{{X, Y}, lists:nth(X, Line)} || X <- lists:seq(1, MaxX)]
                             end,
                             [],
                             LinesWithY),
    dict:from_list(CoordsList).

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

load_input_test() ->
    TestM = load_input("test_input_day12.txt"),
    ?assertEqual(8*5, dict:size(TestM)),

    M = load_input("input_day12.txt"),
    ?assertEqual(95*41, dict:size(M)).

-endif.
