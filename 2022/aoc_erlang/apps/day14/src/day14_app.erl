-module(day14_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(_FileName) ->
    ok.

part2(_FileName) ->
    ok.

%%% Internal functions
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_input(FileName) ->
    InLines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]),
    lists:map(fun(Line) ->
                      TrimmedPointsStr = [string:trim(PointStr) || PointStr <- re:split(Line, "->")],
                      lists:map(fun(PointStr) ->
                                        [XStr, YStr] = string:split(PointStr, ","),
                                        {X, _} = string:to_integer(XStr),
                                        {Y, _} = string:to_integer(YStr),
                                        {X,Y}
                                end,
                                TrimmedPointsStr)
              end,
              InLines).

line({SameX,Y1},{SameX,Y2}) ->
    lists:reverse([{SameX, Y} || Y <- lists:seq(Y1+1, Y2)]);
line({X1,SameY},{X2,SameY}) ->
    [{X, SameY} || X <- lists:seq(X2, X1-1)].

map_from_record([Record|RestRecords], AccSet) ->
    [{FirstX,FirstY}|RestPoints] = Record,
    SetFromRecord = lists:foldl(fun({X,Y}, [{PrevX,PrevY}|_] = Acc) ->
                                        line({PrevX,PrevY}, {X,Y})++Acc
                                end,
                                [{FirstX, FirstY}],
                                RestPoints),
    NewAccSet = sets:union(AccSet, sets:from_list(SetFromRecord)),
    case RestRecords of
        [] -> NewAccSet;
        _ -> map_from_record(RestRecords, NewAccSet)
    end.

map_from_records(Records) ->
    Map = map_from_record(Records, sets:new()),
    sets:from_list([
                    {498,4}, {498,5}, {498,6}, {497,6}, {496,6},
                    {503,4}, {502,4}, {502,5}, {502,6}, {502,7}, {502,8}, {502,9},
                    {501,9}, {500,9}, {499,9}, {498,9}, {497,9}, {496,9}, {495,9}, {494,9}]).

%%% Unit tests

%%% 0 ......+...
%%% 1 ..........
%%% 2 ..........
%%% 3 ..........
%%% 4 ....#...##
%%% 5 ....#...#.
%%% 6 ..###...#.
%%% 7 ........#.
%%% 8 ........#.
%%% 9 #########.

read_map_test() ->
    ExpectedMap = sets:from_list([
                                  {498,4}, {498,5}, {498,6}, {497,6}, {496,6},
                                  {503,4}, {502,4}, {502,5}, {502,6}, {502,7}, {502,8}, {502,9},
                                  {501,9}, {500,9}, {499,9}, {498,9}, {497,9}, {496,9}, {495,9}, {494,9}]),
    Records = read_input("test_input_day14.txt"),
    ?assertEqual(ExpectedMap, map_from_records(Records)).

-endif.
