-module(day14_app).

-export([part1/1, part2/1]).

-define(SAND_SOURCE, {500,0}).

%%% Exported functions

part1(FileName) ->
    RockPositions = read_map(FileName),
    RestingPositions = reach_resting_state_opt_part1(RockPositions, sets:new(), [?SAND_SOURCE], {floor, inf}),
    sets:size(RestingPositions).

part2(FileName) ->
    RockPositions = read_map(FileName),
    MaxY = 1 + lists:max([Y || {_X,Y} <- sets:to_list(RockPositions)]),
    Unvisited = generate_unvisited(?SAND_SOURCE, MaxY, RockPositions),
    Visited = process_unvisited([], Unvisited),
    length(Visited).

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

generate_unvisited({SourceX,SourceY}, MaxY, RockPositions) when SourceY < MaxY->
    Spine = [{SourceX, Y} || Y <- lists:seq(SourceY, MaxY)],
    Ribs = lists:foldl(fun({{CenterX, Y}, Length}, RibsSoFar) ->
                        Edge = Length div 2,
                        MinX = CenterX - Edge,
                        MaxX = CenterX + Edge,
                        NewRib = sets:from_list([{X, Y} || X <- lists:seq(MinX, MaxX)]),
                        sets:union(RibsSoFar, NewRib)
                end,
                sets:new(),
                lists:zip(Spine, lists:seq(1,2*length(Spine), 2))
               ),
    Set = sets:subtract(Ribs, RockPositions),
    MapWithoutSourceSetTo0 = maps:from_list([{Coords, inf} || Coords <- sets:to_list(Set)]),
    maps:update({SourceX,SourceY}, 0, MapWithoutSourceSetTo0).

apply_steps(RockPositions, SandRestingPositions, {X, Y}, Floor = {floor, FloorValY}) ->
    {NewX, NewY} = apply_step(RockPositions, SandRestingPositions, {X, Y}, Floor),

    FloorFun = case FloorValY of
                   inf -> fun() ->
                                  does_flow_out(RockPositions, SandRestingPositions, {NewX, NewY})
                          end;
                   _ -> fun() ->
                                {NewX,NewY} =:= ?SAND_SOURCE
                        end
               end,

    case FloorFun() of
        true ->
            flows_out;
        _ ->
            case {X,Y} =:= {NewX,NewY} orelse {NewX,NewY} =:= ?SAND_SOURCE of
                true -> {X,Y};
                _ ->
                    apply_steps(RockPositions, SandRestingPositions, {NewX, NewY}, Floor)
            end
    end.

apply_steps_opt_part1(RockPositions, SandRestingPositions, [{X, Y}|_] = SandSources) ->
    {NewX, NewY} = apply_step(RockPositions, SandRestingPositions, {X, Y}, {floor, inf}),

    case does_flow_out(RockPositions, SandRestingPositions, {NewX, NewY}) of
        true ->
            flows_out;
        _ ->
            case {X,Y} =:= {NewX,NewY} orelse {NewX,NewY} =:= ?SAND_SOURCE of
                true -> {{X,Y}, tl(SandSources)};
                _ ->
                    apply_steps_opt_part1(RockPositions, SandRestingPositions, [{NewX, NewY}|SandSources])
            end
    end.

reach_resting_state(RockPositions, SandRestingPositions, SandSource, Floor) ->
    case apply_steps(RockPositions, SandRestingPositions, SandSource, Floor) of
        flows_out ->
            SandRestingPositions;
        {X,Y} ->
            %%?debugFmt("Adding new resting coords: ~p, ~p~n", [X, Y]),
            reach_resting_state(RockPositions, sets:add_element({X,Y}, SandRestingPositions), SandSource, Floor)
    end.

reach_resting_state_opt_part1(RockPositions, SandRestingPositions, SandSources, Floor) ->
    case apply_steps_opt_part1(RockPositions, SandRestingPositions, SandSources) of
        flows_out ->
            SandRestingPositions;
        {{X,Y}, NewSandSources} ->
            %%?debugFmt("Adding new resting coords: ~p, ~p~n", [X, Y]),
            reach_resting_state_opt_part1(RockPositions, sets:add_element({X,Y}, SandRestingPositions), NewSandSources, Floor)
    end.

get_floor(RockPositions) ->
    2 + sets:fold(fun({_X,Y}, SoFarMaxY) ->
                          case SoFarMaxY < Y of
                              true -> Y;
                              _ -> SoFarMaxY
                          end
                  end,
                  0,
                  RockPositions).

is_neighbour({X,Y1}, {X,Y2}) when Y1 + 1 =:= Y2 -> true;
is_neighbour({X1,Y1}, {X2,Y2}) when X1-1 =:= X2 andalso Y1 + 1 =:= Y2 -> true;
is_neighbour({X1,Y1}, {X2,Y2}) when X1+1 =:= X2 andalso Y1 + 1 =:= Y2 -> true;
is_neighbour({_,_}, {_,_}) -> false.

neighbours({X,Y}, Map) ->
    maps:filter(fun({Ux, Uy}, _) -> is_neighbour({X,Y}, {Ux,Uy}) end, Map).

process_unvisited_step(VisitedList, UnvisitedMap) ->
    NodesWithTentativeVal = maps:to_list(maps:filter(fun(_, Val) -> Val =/= inf end, UnvisitedMap)),
    {X, Y} = hd(lists:sort([Coords || {Coords, _Val} <- NodesWithTentativeVal])),
    Neihgbours = neighbours({X, Y}, UnvisitedMap),
    NewUnvisited = maps:fold(fun(Coords, _, AccMap) ->
                                      maps:update(Coords, 1, AccMap)
                              end,
                              maps:remove({X,Y}, UnvisitedMap),
                              Neihgbours),
    NewVisited = [{X, Y}|VisitedList],
    {NewVisited, NewUnvisited}.

process_unvisited(Visited, []) -> Visited;
process_unvisited(Visited, Unvisited) ->
    {NewVisited, NewUnvisited} = process_unvisited_step(Visited, Unvisited),
    NonInf = maps:filter(fun(_Key, Val) ->
                                 Val =/= inf
                          end,
                          NewUnvisited),
    case maps:size(NonInf) =:= 0 of
        true -> NewVisited;
        false -> process_unvisited(NewVisited, NewUnvisited)
    end.

%%%
%%% Unit tests
%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_map_test() ->
    ExpectedMap = sets:from_list([
                                  {498,4}, {498,5}, {498,6}, {497,6}, {496,6},
                                  {503,4}, {502,4}, {502,5}, {502,6}, {502,7}, {502,8}, {502,9},
                                  {501,9}, {500,9}, {499,9}, {498,9}, {497,9}, {496,9}, {495,9}, {494,9}]),
    ReadMap = read_map("test_input_day14.txt"),
    ?assertEqual(lists:sort(sets:to_list(ExpectedMap)), lists:sort(sets:to_list(ReadMap))).

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

%%% TODO: END: Use test setups for readint input for below 3,4 tests

apply_steps_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    ?assertEqual({500,8}, apply_steps(RockPositions, sets:new(), ?SAND_SOURCE, {floor, inf})).

reach_resting_state_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    ExpectedRestingPositions = sets:from_list([{500,2},
                                               {499,3},{500,3},{501,3},
                                               {499,4},{500,4},{501,4},
                                               {497,5},{499,5},{500,5},{501,5},
                                               {499,6},{500,6},{501,6},
                                               {498,7},{499,7},{500,7},{501,7},
                                               {495,8},{497,8},{498,8},{499,8},{500,8},{501,8}]),
    RestingPositions = reach_resting_state(RockPositions, sets:new(), ?SAND_SOURCE, {floor, inf}),

    SortedExpected = lists:sort(sets:to_list(ExpectedRestingPositions)),
    SortedEvaluated = lists:sort(sets:to_list(RestingPositions)),

    ?assertEqual(lists:sort(sets:to_list(ExpectedRestingPositions)), lists:sort(sets:to_list(RestingPositions))).

part1_test_() ->
    {timeout, 15*60,
     fun() ->
             TestResult = part1("test_input_day14.txt"),
             ?debugFmt("Part1 test result: ~p~n", [TestResult]),
             ?assertEqual(24, TestResult),

             Result = part1("input_day14.txt"),
             ?debugFmt("Part1 result: ~p~n", [Result]),
             ?assertEqual(1133, Result),
             ok
     end}.

%%% Part2 work in progress

get_floor_test() ->
    RockPositions = read_map("test_input_day14.txt"),
    FloorY = get_floor(RockPositions),
    ?assertEqual(11, FloorY).

%%% Part2: optimization

generate_unvisited_test() ->
    RockPositions = sets:from_list([{499,1}, {499,2}, {500,2}, {499,3}]),
    Unvisited = generate_unvisited(?SAND_SOURCE, 3, RockPositions),
    ExpectedUnvisitedWithoutSandSource = maps:from_list([{Coords,inf} || Coords <-
                                                                         [?SAND_SOURCE,
                                                                          {500,1}, {501,1},
                                                                          {498,2}, {501,2}, {502,2},
                                                                          {497,3}, {498,3}, {500,3}, {501,3}, {502,3}, {503,3}]]),
    ExpectedUnvisited = maps:update(?SAND_SOURCE, 0, ExpectedUnvisitedWithoutSandSource),
    ?assertEqual(ExpectedUnvisited, Unvisited).


%%% TODO: .............
neighbours_test() ->
    Coords = maps:from_list([{{1,1},inf},{{2,1},inf},{{3,1},inf},
                             {{1,2},inf},{{2,2},inf},{{3,2},inf},
                             {{1,3},inf},{{2,3},inf},{{3,3},inf}]),
    ExpectedNeighbours = maps:from_list([{{1,3},inf},{{2,3},inf},{{3,3},inf}]),
    ?assertEqual(ExpectedNeighbours, neighbours({2,2}, Coords)).

process_unvisited_step_test() ->
    RockPositions = sets:from_list([{499,1}, {499,2}, {500,2}, {499,3}]),
    Unvisited = generate_unvisited(?SAND_SOURCE, 3, RockPositions),
    Visited = [],
    {NewVisited, NewUnvisited} = process_unvisited_step(Visited, Unvisited),
    ExpectedVisited = [?SAND_SOURCE],
    ExpectedUnvisited = maps:update({500,1}, 1,
                                    maps:update({501, 1}, 1,
                                                maps:remove(?SAND_SOURCE, Unvisited))),
    ?assertEqual(ExpectedVisited, NewVisited),
    ?assertEqual(ExpectedUnvisited, NewUnvisited).

part2_test_() ->
    {timeout, 3*60*60,
     fun() ->
             TestResult = part2("test_input_day14.txt"),
             ?debugFmt("Part2 test result: ~p~n", [TestResult]),
             ?assertEqual(93, TestResult),

             Result = part2("input_day14.txt"),
             ?debugFmt("Part2 result: ~p~n", [Result]),
             ?assertEqual(27566, Result),
             ok
     end}.

-endif.
