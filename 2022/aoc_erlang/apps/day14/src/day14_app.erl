-module(day14_app).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    RockPositions = read_map(FileName),
    RestingPositions = reach_resting_state(RockPositions, sets:new(), {500, 0}, {floor, inf}),
    sets:size(RestingPositions).

part2(FileName) ->
    RockPositions = read_map(FileName),
    FloorValY = get_floor(RockPositions),
    RestingPositions = reach_resting_state(RockPositions, sets:new(), {500, 0}, {floor, FloorValY}),
    ?debugFmt("FloorValY: ~p~nRestingPositions: ~p", [FloorValY, lists:sort(sets:to_list(RestingPositions))]),
    1 + sets:size(RestingPositions). %%% 1 is SandSource

%%% Internal functions

read_map(FileName) ->
    map_from_records(read_input(FileName)).

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

dir(V1,V2) when V1 > V2 -> -1;
dir(V1,V2) when V1 =< V2 -> 1.

line({SameX,Y1},{SameX,Y2}) ->
    lists:reverse([{SameX, Y} || Y <- lists:seq(Y1, Y2, dir(Y1, Y2))]);
line({X1,SameY},{X2,SameY}) ->
    [{X, SameY} || X <- lists:seq(X2, X1, dir(X2, X1))].

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
    map_from_record(Records, sets:new()).

read_map_test() ->
    ExpectedMap = sets:from_list([
                                  {498,4}, {498,5}, {498,6}, {497,6}, {496,6},
                                  {503,4}, {502,4}, {502,5}, {502,6}, {502,7}, {502,8}, {502,9},
                                  {501,9}, {500,9}, {499,9}, {498,9}, {497,9}, {496,9}, {495,9}, {494,9}]),
    ReadMap = read_map("test_input_day14.txt"),
    ?assertEqual(lists:sort(sets:to_list(ExpectedMap)), lists:sort(sets:to_list(ReadMap))).

apply_step(RockPositions, SandRestingPositions, {X, Y}, {floor, FloorValY}) ->
    Combined = sets:union(RockPositions, SandRestingPositions),

    case reaches_floor({X,Y}, FloorValY) of
        true ->
            {X,Y};
        _ ->
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
            end
    end.

does_flow_out(RockPositions, SandRestingPositions, {X, Y}) ->
    Combined = sets:union(RockPositions, SandRestingPositions),
    BlockingPixels = sets:filter(fun({ThisX, ThisY}) ->
                                         X =:= ThisX andalso Y < ThisY
                                 end,
                                 Combined),
    sets:is_empty(BlockingPixels).

reaches_floor({_X, Y}, FloorValY) ->
    Y + 1 >= FloorValY.

%%% TODO: BEGIN: Use test setups for readint input for below 3,4 tests
step_down_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:new(),
    ?assertEqual({500, 1}, apply_step(RockPositions, SandRestingPositions, {500, 0}, {floor, inf})).

step_down_left_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:new(),
    ?assertEqual({501, 4}, apply_step(RockPositions, SandRestingPositions, {502, 3}, {floor, inf})).

step_down_right_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:from_list([{497,4}]),
    ?assertEqual({499, 4}, apply_step(RockPositions, SandRestingPositions, {498, 3}, {floor, inf})).

step_into_rest_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    SandRestingPositions = sets:from_list([{497,4}, {499,4}]),
    ?assertEqual({498, 3}, apply_step(RockPositions, SandRestingPositions, {498, 3}, {floor, inf})).

flows_out_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    ?assertEqual(false, does_flow_out(RockPositions, sets:new(), {500, 1})),
    ?assertEqual(false, does_flow_out(RockPositions, sets:from_list([{508, 8}, {499,8}, {501,8}, {500,7}]), {498, 8})),
    ?assertEqual(true, does_flow_out(RockPositions, sets:new(), {503, 5})).

reaches_floor_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    FloorValY = 11,
    ?assertEqual(false, reaches_floor({503, 5}, FloorValY)),
    ?assertEqual(true, reaches_floor({503, 10}, FloorValY)).

%%%
%%%     4  4  5  5
%%%     9  9  0  0
%%%     4  7  0  3
%%%  0  ......+...
%%%  1  ..........
%%%  2  ..........
%%%  3  ..........
%%%  4  ....#...##
%%%  5  ....#...#.
%%%  6  ..###...#.
%%%  7  ........#.
%%%  8  ........#.
%%%  9  #########.
%%% 10  ..........
%%% 11  ##########

%%%    4  4  5  5
%%%    9  9  0  0
%%%    4  7  0  3
%%% 0 .......+...
%%% 1 .......~...
%%% 2 ......~o... {},
%%% 3 .....~ooo.. {499,3},{500,3},{501,3},
%%% 4 ....~#ooo## {499,4},{500,4},{501,4},
%%% 5 ...~o#ooo#. {497,5},{499,5},{500,5},{501,5},
%%% 6 ..~###ooo#. {499,6},{500,6},{501,6},
%%% 7 ..~..oooo#. {498,7},{499,7},{500,7},{501,7},
%%% 8 .~o.ooooo#. {495,8},{497,8},{498,8},{499,8},{500,8},{501,8},
%%% 9 ~#########.
%%%   ~..........
%%%   ~..........
%%%   ~..........
%%%
%%%

%%% TODO: END: Use test setups for readint input for below 3,4 tests

apply_steps(RockPositions, SandRestingPositions, {X, Y}, Floor = {floor, FloorValY}) ->
    {NewX, NewY} = apply_step(RockPositions, SandRestingPositions, {X, Y}, Floor),

    FloorFun = case FloorValY of
                   inf -> fun() ->
                                  does_flow_out(RockPositions, SandRestingPositions, {NewX, NewY})
                          end;
                   _ -> fun() ->
                                {NewX,NewY} =:= {500,0}
                        end
               end,

    case FloorFun() of 
        true ->
            ?debugFmt("{~p, ~p, ~p} FloorFun() flows_out~n", [X, Y, FloorValY]),
            flows_out;
        _ ->
            case {X,Y} =:= {NewX,NewY} orelse {NewX,NewY} =:= {500,0} of
                true -> {X,Y};
                _ ->
                    apply_steps(RockPositions, SandRestingPositions, {NewX, NewY}, Floor)
            end
    end.

apply_steps_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    ?assertEqual({500,8}, apply_steps(RockPositions, sets:new(), {500,0}, {floor, inf})).

reach_resting_state(RockPositions, SandRestingPositions, SandSource, Floor) ->
    case apply_steps(RockPositions, SandRestingPositions, SandSource, Floor) of
        flows_out ->
            SandRestingPositions;
        {X,Y} ->
            %%?debugFmt("Adding new resting coords: ~p, ~p~n", [X, Y]),
            reach_resting_state(RockPositions, sets:add_element({X,Y}, SandRestingPositions), SandSource, Floor)
    end.

reach_resting_state_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    ExpectedRestingPositions = sets:from_list([{500,2},
                                               {499,3},{500,3},{501,3},
                                               {499,4},{500,4},{501,4},
                                               {497,5},{499,5},{500,5},{501,5},
                                               {499,6},{500,6},{501,6},
                                               {498,7},{499,7},{500,7},{501,7},
                                               {495,8},{497,8},{498,8},{499,8},{500,8},{501,8}]),
    RestingPositions = reach_resting_state(RockPositions, sets:new(), {500,0}, {floor, inf}),

    SortedExpected = lists:sort(sets:to_list(ExpectedRestingPositions)),
    SortedEvaluated = lists:sort(sets:to_list(RestingPositions)),

    ?assertEqual(lists:sort(sets:to_list(ExpectedRestingPositions)), lists:sort(sets:to_list(RestingPositions))).

part1_test_() ->
    {timeout, 15*60,
     fun() ->
             TestResult = part1("test_input_day14.txt"),
             ?debugFmt("Part1 test result: ~p~n", [TestResult]),
             ?assertEqual(24, TestResult),

             %%% TODO: optimize this part
             %Result = part1("input_day14.txt"),
             %?debugFmt("Part1 result: ~p~n", [Result]),
             %?assertEqual(1133, Result),
             ok
     end}.

%%% Part2 work in progress

get_floor(RockPositions) ->
    2 + sets:fold(fun({X,Y}, SoFarMaxY) ->
                          case SoFarMaxY < Y of
                              true -> Y;
                              _ -> SoFarMaxY
                          end
                  end,
                  0,
                  RockPositions).

get_floor_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    FloorY = get_floor(RockPositions),
    ?assertEqual(11, FloorY).

part2_test_() ->
    {timeout, 3*60*60,
     fun() ->
             TestResult = part2("test_input_day14.txt"),
             ?debugFmt("Part2 test result: ~p~n", [TestResult]),
             ?assertEqual(93, TestResult),

             %%% TODO: optimize this part
             Result = part2("input_day14.txt"),
             ?debugFmt("Part2 result: ~p~n", [Result]),
             ?assertEqual(1, Result),
             ok
     end}.

%%% Part2 work in progress

-endif.
%%%
%%% Unit tests

