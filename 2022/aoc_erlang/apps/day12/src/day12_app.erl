-module(day12_app).

-export([part1/1, part2/1]).

%%% Exported functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

part1(FileName) ->
    M = load_input(FileName),
    find_fewest_steps(M).

part1_new(FileName) ->
    M = load_input(FileName),
    Visited = evaluate_paths(M), % TODO: pass StartPoint to evaluate_paths, EndPoint is probably not needed at all since we'd like to evaluate all point with regard to shortest path

    ?debugFmt("Visited: ~p~n", [dict:to_list(Visited)]),

    {EndX, EndY} = find($S, M),
    dict:find({EndX,EndY}, Visited).

part2(FileName) ->
    M = load_input(FileName),
    AllLowest = get_all_for_height($a, M),
    Size = dict:size(AllLowest),
    {Result, _Cnt} = dict:fold(fun({X,Y},_,{Acc, Cnt}) ->
                                       %%io:format("Curr Acc: ~p (progress: ~p/~p)", [Acc, Cnt, Size]),
                                       ?debugFmt("Curr Acc: ~p (progress: ~p/~p)", [Acc, Cnt, Size]),
                                       ModM = replace_start({X,Y}, M),
                                       Out = find_fewest_steps(ModM),
                                       case Out of
                                           {ok, StepsNo} ->
                                               case StepsNo < Acc of
                                                   true ->
                                                       {StepsNo, Cnt+1};
                                                   _ -> {Acc, Cnt+1}
                                               end;
                                           error -> {Acc, Cnt+1}
                                       end
                               end,
                               {find_fewest_steps(M), 0},
                               AllLowest),
    Result.

part2_new(FileName) ->
    M = load_input(FileName),
    AllLowest = get_all_for_height($a, M),
    Size = dict:size(AllLowest),
    Visited = evaluate_paths(M),
    VisitedFilteredList = lists:filter(fun({{X,Y}, _Val}) ->
                                               dict:is_key({X,Y}, AllLowest)
                                       end,
                                       dict:to_list(Visited)),
    SortedVisited = lists:sort(fun({_Key1, Val1}, {_Key2, Val2}) ->
                                       Val1 =< Val2
                               end,
                               VisitedFilteredList),
    ?debugFmt("SortedVisited: ~p", [SortedVisited]),
    {_, Result} = hd(SortedVisited),
    Result.

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

find_fewest_steps(M) -> % TODO: to be deleted after evaluate_paths is fully implemented and tested
    {StartX, StartY} = find($E, M),
    {EndX, EndY} = find($S, M),
    Unvisited = dict:store({StartX,StartY}, 0, init_unvisited(M)),
    Visited = dict:new(),
    FinalVisited = find_fewest_steps(Visited, Unvisited, M, {EndX, EndY}),
    dict:find({EndX,EndY}, FinalVisited).

find_fewest_steps(Visited, Unvisited, M, Endpoint) -> % TODO: to be deleted after evaluate_paths is fully implemented and tested
    case only_inf_in_unvisited(Unvisited) or endpoint_in_visited(Endpoint, Visited) of
        true ->
            Visited;
        _ -> case dict:size(Unvisited) of
                 0 -> Visited;
                 _ ->
                     {NewVisited, NewUnvisited} = process_point(Visited, Unvisited, M),
                     %%?debugFmt("NewVisited: ~p", [dict:to_list(NewVisited)]),
                     find_fewest_steps(NewVisited, NewUnvisited, M, Endpoint)
             end
    end.

evaluate_paths(M) ->
    {StartX, StartY} = find($E, M),
    {EndX, EndY} = find($S, M),
    Unvisited = dict:store({StartX,StartY}, 0, init_unvisited(M)),
    Visited = dict:new(),
    FinalVisited = find_fewest_steps(Visited, Unvisited, M, {EndX, EndY}).

evaluate_paths(Visited, Unvisited, M, Endpoint) ->
    case only_inf_in_unvisited(Unvisited) of
        true ->
            Visited;
        _ -> case dict:size(Unvisited) of
                 0 -> Visited;
                 _ ->
                     {NewVisited, NewUnvisited} = process_point(Visited, Unvisited, M),
                     %%?debugFmt("NewVisited: ~p", [dict:to_list(NewVisited)]),
                     find_fewest_steps(NewVisited, NewUnvisited, M, Endpoint)
             end
    end.

get_all_for_height(Height, M) ->
    dict:filter(fun({X,Y}, _) ->
                        ThisPointHeight = height({X,Y}, M),
                        ThisPointHeight =:= Height
                end,
                M).

replace_start({X,Y}, M) ->
    {OldStartX, OldStartY} = find($S, M),
    M2 = dict:store({OldStartX, OldStartY}, $a, M),
    %%?debugFmt("{OldStartX, OldStartY}: {~p,~p}", [OldStartX,OldStartY]),
    %%?debugFmt("{NewStartX, NewStartY}: {~p,~p}", [X,Y]),
    Result = dict:store({X,Y}, $S, M2),
    %%?debugFmt("Result: ~p", [dict:to_list(Result)]),
    Result.

find(Symbol, M) ->
    Filtered = dict:filter(fun(_Key, Val) ->
                                  Val =:= Symbol
                           end,
                           M),
    [{{X,Y}, _Val}] = dict:to_list(Filtered),
    {X,Y}.

init_unvisited(M) ->
    dict:fold(fun(Key, _Val, Acc) ->
                      dict:store(Key, inf, Acc)
              end,
              dict:new(),
              M).

height({X,Y}, M) ->
    case dict:fetch({X,Y}, M) of
        $S -> $a;
        $E -> $z;
        _ -> dict:fetch({X,Y}, M)
    end.

only_inf_in_unvisited(Unvisited) ->
    lists:all(fun({{_,_}, Val}) ->
                      Val =:= inf
              end,
              dict:to_list(Unvisited)).

endpoint_in_visited(Endpoint, Visited) -> % TODO: might not be used if evaluate_paths is fully implemented and tested
    dict:is_key(Endpoint, Visited).

process_point(Visited, Unvisited, M) ->
    {X,Y} = get_next_point(Unvisited),
    Neighbours = neighbours_for_point({X,Y}, Unvisited, M),
    Dist = dict:fetch({X,Y}, Unvisited),
    PotentialNewDist = Dist + 1,
    NewUnvisited = lists:foldl(fun({Nx, Ny}, Acc) ->
                                       StoredDist = dict:fetch({Nx,Ny}, Acc),
                                       case PotentialNewDist < StoredDist of
                                           true -> dict:store({Nx,Ny}, PotentialNewDist, Acc);
                                           _ -> Acc
                                       end
                               end,
                               Unvisited,
                               Neighbours),
    NewVisited = dict:store({X,Y}, Dist, Visited),
    {NewVisited, dict:erase({X,Y}, NewUnvisited)}.

get_next_point(Unvisited) ->
    {Point, _Dist} = dict:fold(fun({X,Y}, Dist, {}) ->
                                       {{X,Y}, Dist};
                                  ({X,Y}, Dist, {{StoredX,StoredY}, StoredDist}) ->
                                       case Dist < StoredDist of
                                           true -> {{X,Y}, Dist};
                                           _ ->
                                               {{StoredX, StoredY}, StoredDist}
                                       end
                               end,
                               {},
                               Unvisited),
    Point.

is_there_pass_between_points ({X1,Y1}, {X2,Y2}, M) ->
    H1 = height({X1,Y1}, M),
    H2 = height({X2,Y2},M),
    H2 - H1 =< 1.

is_there_pass_between_points_for_reversed_paths ({X1,Y1}, {X2,Y2}, M) -> %%% TODO: not yest used in resolutions
    is_there_pass_between_points({X2,Y2}, {X1,Y1}, M).

neighbours_for_point({X,Y}, Unvisited, M) ->
    CartesianNeighbours = [{X+1,Y}, {X-1,Y}, {X,Y+1}, {X,Y-1}],
    WithinChartNeighbours = lists:filter(fun({NeighbourX,NeighbourY}) ->
                                                 dict:is_key({NeighbourX, NeighbourY}, M)
                                         end,
                                         CartesianNeighbours),
    UnvisitedNeighbours = lists:filter(fun({NeighbourX,NeighbourY}) ->
                                               dict:is_key({NeighbourX,NeighbourY}, Unvisited)
                                       end,
                                       WithinChartNeighbours),
    _ConnectedNeighbours = lists:filter(fun({NeighbourX,NeighbourY}) ->
                                                is_there_pass_between_points_for_reversed_paths({X,Y}, {NeighbourX,NeighbourY}, M)
                                        end,
                                        UnvisitedNeighbours).

%%% Unit tests
%%-ifdef(TEST).
%%-include_lib("eunit/include/eunit.hrl").

load_input_test() ->
    TestM = load_input("test_input_day12.txt"),
    ?assertEqual(8*5, dict:size(TestM)),

    M = load_input("input_day12.txt"),
    ?assertEqual(95*41, dict:size(M)).

neighbours_for_point_test() ->
    M = load_input("test_input_day12.txt"),
    Unvisited = init_unvisited(M),

    ExpectedNeighbours1 = sets:from_list([{2,1}, {1,2}]),
    ?assertEqual(ExpectedNeighbours1, sets:from_list(neighbours_for_point({1,1}, Unvisited, M))),

    ExpectedNeighbours2 = sets:from_list([{5,3}]),
    ?assertEqual(ExpectedNeighbours2, sets:from_list(neighbours_for_point({6,3}, Unvisited, M))),

    ExpectedNeighbours3 = sets:from_list([{1,5}, {3,5}, {2,4}]),
    ?assertEqual(ExpectedNeighbours3, sets:from_list(neighbours_for_point({2,5}, Unvisited, M))),

    ExpectedNeighbours4 = sets:from_list([{4,1}, {5,2}, {4,3}]),
    ?assertEqual(ExpectedNeighbours4, sets:from_list(neighbours_for_point({4,2}, Unvisited, M))).

init_unvisited_test() ->
    M = load_input("test_input_day12.txt"),
    InitDist = init_unvisited(M),
    dict:map(fun(Key, Value) ->
                     ?assertEqual(inf, Value)
             end,
             InitDist).

process_point_test() ->
    M = load_input("test_input_day12.txt"),
    Unvisited = dict:store({4,2}, 5, init_unvisited(M)),
    Visited = dict:new(),
    ExepectedNewVisited = dict:from_list([{{4,2},5}]),

    ExpUnvisitedPrep1 = dict:erase({4,2}, Unvisited),
    ExpUnvisitedPrep2 = dict:store({4,1}, 6, ExpUnvisitedPrep1),
    ExpUnvisitedPrep3 = dict:store({5,2}, 6, ExpUnvisitedPrep2),
    ExpUnvisitedPrep4 = dict:store({4,3}, 6, ExpUnvisitedPrep3),
    ExepectedNewUnvisited = ExpUnvisitedPrep4,

    {NewVisited, NewUnvisited} = process_point(Visited, Unvisited, M),
    ?assertEqual(ExepectedNewVisited, NewVisited),
    ?assertEqual(ExepectedNewUnvisited, NewUnvisited).

get_next_point_test() ->
    M = load_input("test_input_day12.txt"),
    UnvisitedPrep = dict:store({3,2}, 5, init_unvisited(M)),
    Unvisited = dict:store({7,3}, 3, init_unvisited(M)),
    {NextX, NextY} = get_next_point(Unvisited),
    ?assertEqual({7,3}, {NextX, NextY}).

find_fewest_steps_test() ->
    M = load_input("test_input_day12.txt"),
    ?assertEqual({ok, 31}, find_fewest_steps(M)).

get_all_for_level_test() ->
    M = load_input("test_input_day12.txt"),
    AllLowest = get_all_for_height($a, M),
    ?assertEqual(6, dict:size(AllLowest)).


part1_test() ->
    TestResult = part1("test_input_day12.txt"),
    ?debugFmt("Part1 result: ~p", [TestResult]),
    ?assertEqual({ok,31}, TestResult),

    Result = part1("input_day12.txt"),
    ?debugFmt("Part1 result: ~p", [Result]),
    ?assertEqual({ok,420}, Result).

part1_new_test() ->
    TestResult = part1_new("test_input_day12.txt"),
    ?debugFmt("Part1 result: ~p", [TestResult]),
    ?assertEqual({ok,31}, TestResult),

    Result = part1_new("input_day12.txt"),
    ?debugFmt("Part1 result: ~p", [Result]),
    ?assertEqual({ok,420}, Result).

part2_test_() ->
    %% TODO: this is very not optimal
    %%       we can have this result if we properly process whole matrix for part1 and insted only result for S
    %%       we will have results for all matrix fileds, then we can filter matrix only for a and then get minimal distance
    {timeout, 60*60, ?_test(begin
                             TestResult = part2("test_input_day12.txt"),
                             ?debugFmt("TestResult: ~p", [TestResult]),
                             ?assertEqual(29, TestResult),

                             Result = part2("input_day12.txt"),
                             ?debugFmt("Result: ~p", [Result]),
                             ?assertEqual(414, Result)
                         end)}.

part2_new_test() ->
    TestResultNew = part2_new("test_input_day12.txt"),
    ?debugFmt("TestResult: ~p", [TestResultNew]),
    ?assertEqual(29, TestResultNew),

    Result = part2_new("input_day12.txt"),
    ?debugFmt("Result: ~p", [Result]),
    ?assertEqual(414, Result).

-endif.
