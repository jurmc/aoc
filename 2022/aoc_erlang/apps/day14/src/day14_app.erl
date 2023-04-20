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

read_map(FileName) ->
    map_from_records(read_input("test_input_day14.txt")).

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

read_map_test() ->
    ExpectedMap = sets:from_list([
                                  {498,4}, {498,5}, {498,6}, {497,6}, {496,6},
                                  {503,4}, {502,4}, {502,5}, {502,6}, {502,7}, {502,8}, {502,9},
                                  {501,9}, {500,9}, {499,9}, {498,9}, {497,9}, {496,9}, {495,9}, {494,9}]),
    ?assertEqual(ExpectedMap, read_map("test_input_day14.txt")).

apply_step(RockPositions, SandRestingPositions, {X, Y}) ->
    Combined = sets:union(RockPositions, SandRestingPositions),
    case sets:is_element({X,Y+1}, Combined) of
        true ->
            case sets:is_element({X-1,Y+1}, Combined) of
                true ->
                    case sets:is_element({X+1,Y+1}, Combined) of
                        true ->
                            {X,Y};
                        _ ->
                            {X+1, Y+1}
                    end;
                _ ->
                    {X-1,Y+1}
            end;
        _ ->
            {X, Y+1}
    end.

does_flow_out(RockPositions, SandRestingPositions, {X, Y}) ->
    Combined = sets:union(RockPositions, SandRestingPositions),
    BlockingPixels = sets:filter(fun({ThisX, ThisY}) ->
                                         X =:= ThisX andalso Y > ThisY
                                 end,
                                 Combined),
    not sets:is_empty(BlockingPixels).

%%% TODO: BEGIN: Use test setups for readint input for below 3,4 tests
step_down_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:new(),
    ?assertEqual({500, 1}, apply_step(RockPositions, SandRestingPositions, {500, 0})).

step_down_left_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:new(),
    ?assertEqual({501, 4}, apply_step(RockPositions, SandRestingPositions, {502, 3})).

step_down_right_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:from_list([{497,4}]),
    ?assertEqual({499, 4}, apply_step(RockPositions, SandRestingPositions, {498, 3})).

step_into_rest_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:from_list([{497,4}, {499,4}]),
    ?assertEqual({498, 3}, apply_step(RockPositions, SandRestingPositions, {498, 3})).

flows_out_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    ?assertEqual(false, does_flow_out(RockPositions, sets:new(), {500, 1})),
    ?assertEqual(true, does_flow_out(RockPositions, sets:new(), {503, 5})).

%%% TODO: END: Use test setups for readint input for below 3,4 tests

-endif.

%%%
%%% Unit tests
%%%
%%%   4  4  5  5
%%%   9  9  0  0
%%%   4  7  0  3
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

