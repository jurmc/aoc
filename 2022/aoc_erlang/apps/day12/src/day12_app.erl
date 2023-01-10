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

height({X,Y}, M) ->
    case dict:fetch({X,Y}, M) of
        $S -> $a;
        $E -> $z;
        _ -> dict:fetch({X,Y}, M)
    end.

neighbours_for_point({X,Y}, M) ->
    PotentialNeighbours = [{X+1,Y}, {X-1,Y}, {X,Y+1}, {X,Y-1}],
    PotentialNeighbours1 = lists:filter(fun({NeighbourX,NeighbourY}) ->
                                                dict:is_key({NeighbourX, NeighbourY}, M)
                                        end,
                                        PotentialNeighbours),
    CurrHeight = height({X,Y}, M),
    lists:filter(fun({NeighbourX,NeighbourY}) ->
                         NeighbourHeight = height({NeighbourX,NeighbourY},M),
                         NeighbourHeight - CurrHeight =< 1
                 end,
                 PotentialNeighbours1).

neighbours_for_point_test() ->
    M = load_input("test_input_day12.txt"),

    ExpectedNeighbours1 = sets:from_list([{2,1}, {1,2}]),
    ?assertEqual(ExpectedNeighbours1, sets:from_list(neighbours_for_point({1,1}, M))),

    ExpectedNeighbours2 = sets:from_list([{5,3}, {7,3}, {6,2}, {6,4}]),
    ?assertEqual(ExpectedNeighbours2, sets:from_list(neighbours_for_point({6,3}, M))),

    ExpectedNeighbours3 = sets:from_list([{1,5}, {2,4}]),
    ?assertEqual(ExpectedNeighbours3, sets:from_list(neighbours_for_point({2,5}, M))).

init_unvisited(M) ->
    TentativeDistVals = dict:fold(fun(Key, _Val, Acc) ->
                                          dict:store(Key, inf, Acc)
                                  end,
                                  dict:new(),
                                  M).

init_unvisited_test() ->
    M = load_input("test_input_day12.txt"),
    InitDist = init_unvisited(M),
    dict:map(fun(Key, Value) ->
                     ?assertEqual(inf, Value)
             end,
             InitDist).

process_point(Visited, Unvisited, M) ->
    {X, Y} = get_next_point(Unvisited),
    Neighbours = neighbours_for_point({X,Y}, M),
    Dist = dict:fetch({X,Y}, Unvisited),
    NewUnvisited = lists:foldl(fun({Nx, Ny}, Acc) ->
                                       NewDist = Dist+1,
                                       Val = dict:fetch({Nx,Ny}, Acc),
                                       case Val of
                                           inf -> dict:store({Nx,Ny}, NewDist, Acc);
                                           _ -> dict:update_counter({Nx,Ny}, NewDist, Acc)
                                       end
                               end,
                               Unvisited,
                               Neighbours),
    NewVisited = dict:store({X,Y}, Dist, Visited),
    {NewVisited, dict:erase({X,Y}, NewUnvisited)}.

process_point_test() ->
    M = load_input("test_input_day12.txt"),
    Unvisited = dict:store({2,5}, 5, init_unvisited(M)),
    Visited = dict:new(),
    ExepectedNewVisited = dict:from_list([{{2,5},5}]),

    ExpUnvisitedPrep1 = dict:erase({2,5}, Unvisited),
    ExpUnvisitedPrep2 = dict:store({1,5}, 6, ExpUnvisitedPrep1),
    ExpUnvisitedPrep3 = dict:store({2,4}, 6, ExpUnvisitedPrep2),
    ExepectedNewUnvisited = ExpUnvisitedPrep3,

    {NewVisited, NewUnvisited} = process_point(Visited, Unvisited, M),
    ?assertEqual(ExepectedNewVisited, NewVisited),
    ?assertEqual(ExepectedNewUnvisited, NewUnvisited).

get_next_point(Unvisited) ->
    {Point, _Dist} = dict:fold(fun({X,Y}, Dist, {{StoredX,StoredY}, StoredDist}) ->
                                       case Dist < StoredDist of
                                           true -> {{X,Y}, Dist};
                                           _ -> {{StoredX, StoredY}, StoredDist}
                                       end
                               end,
                               {{1,1}, inf},
                               Unvisited),
    Point.

get_next_point_test() ->
    M = load_input("test_input_day12.txt"),
    UnvisitedPrep = dict:store({3,2}, 5, init_unvisited(M)),
    Unvisited = dict:store({7,3}, 3, init_unvisited(M)),
    {NextX, NextY} = get_next_point(Unvisited),
    ?assertEqual({7,3}, {NextX, NextY}).

find_fewest_steps(M) ->
    Unvisited = dict:store({1,1}, 0, init_unvisited(M)),
    Visited = dict:new(),
    FinalVisited = find_fewest_steps(Visited, Unvisited, M),
    dict:fetch({6,3}, FinalVisited).

find_fewest_steps(Visited, Unvisited, M) ->
    case dict:size(Unvisited) of
        0 -> Visited;
        _ ->
            {NewVisited, NewUnvisited} = process_point(Visited, Unvisited, M),
            find_fewest_steps(NewVisited, NewUnvisited, M)
    end.

find_fewest_steps_test() ->
    M = load_input("test_input_day12.txt"),
    ?assertEqual(31, find_fewest_steps(M)).

-endif.
