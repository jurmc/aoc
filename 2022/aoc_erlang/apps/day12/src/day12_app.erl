-module(day12_app).

-record(matrix, {max_x, max_y, coords}).

%%-export([part1/1, part2/1]).
-compile(export_all).

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%% Exported functions

part2(_FileName) ->
    ok.

part1(_FileName) ->
    ok.

%%% Internal functions

get_ends(CoordsList) ->
    {value, {Begin, _}} = lists:search(fun({_, Val}) -> Val =:= $S end, CoordsList),
    {value, {End, _}} = lists:search(fun({_, Val}) -> Val =:= $E end, CoordsList),
    {Begin, End}.

into_char(Value) ->
    Value + $a.

into_val(Char) -> %% TODO: use character literals instead of deifines
    case Char of
        $S -> 0;
        $E -> 25;
        _ -> Char - $a
    end.

normalize_height(CoordsList) ->
    lists:map(fun({{X, Y}, Val}) -> {{X, Y}, into_val(Val)} end, CoordsList).

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
    {Begin, End} = get_ends(CoordsList),
    NormalizedCoordsList = normalize_height(CoordsList),
    CoordsDict = dict:from_list(NormalizedCoordsList),
    M = #matrix{max_x=MaxX, max_y=MaxY, coords=CoordsDict},
    {Begin, End, M}.

load_input_test() ->
    {TestBegin, TestEnd, TestM} = load_input("test_input_day12.txt"),
    ?assertEqual({1, 1}, TestBegin),
    ?assertEqual({6, 3}, TestEnd),
    ?assertEqual(8, TestM#matrix.max_x),
    ?assertEqual(5, TestM#matrix.max_y),
    ?assertEqual(8*5, dict:size(TestM#matrix.coords)),

    {Begin, End, M} = load_input("input_day12.txt"),
    ?assertEqual({1, 21}, Begin),
    ?assertEqual({73, 21}, End),
    ?assertEqual(95, M#matrix.max_x),
    ?assertEqual(41, M#matrix.max_y),
    ?assertEqual(95*41, dict:size(M#matrix.coords)).

normalize_height_test() ->
    InputList = [{{1,1}, $a},
                 {{2,1}, $b},
                 {{1,2}, $c},
                 {{2,2}, $c}],
    ExpectedList = [{{1,1}, 0},
                    {{2,1}, 1},
                    {{1,2}, 2},
                    {{2,2}, 2}],
    ?assertEqual(ExpectedList, normalize_height(InputList)).

filter_directions({X, Y}, PotentialDirections, HeightMap, CurrPath) ->
    %% Do not go over map edge
    F1 = lists:filter(fun({X2,Y2}) ->
                              dict:is_key({X2,Y2}, HeightMap)
                      end,
                      PotentialDirections),
    %% Do not go to a field that is already in our path
    F2 = lists:filter(fun(NextPosition) -> not lists:member(NextPosition, CurrPath) end, F1),
    %% Do not step onto field that is too high
    {ok, CurrHeight} = dict:find({X, Y}, HeightMap),
    F3 = lists:filter(fun(NextPosition) ->
                              {ok, NextPositionHeight} = dict:find(NextPosition, HeightMap),
                              %%?debugFmt("~nNextPosition: ~p~nCurrHeight: ~p, NextPositionHeight: ~p~n", [NextPosition, CurrHeight, NextPositionHeight]),
                              CurrHeight + 1 >= NextPositionHeight
                      end,
                      F2),
    %%?debugFmt("~nPotentialDirections: ~p~nF1: ~p~nF2: ~p~nF3: ~p~n", [PotentialDirections, F1, F2, F3]),
    F3.

filter_directions_test() ->
    {X,Y} = {1,1},
    HeightMap = dict:from_list([{{1,1}, 0},
                             {{2,1}, 1},
                             {{1,2}, 2},
                             {{2,2}, 2}]),
    PotentialDirections = [{X+1, Y},
                           {X-1, Y},
                           {X, Y+1},
                           {X, Y-1}],
    CurrPath = [{X, Y+1}],
    AllowedDirections = [{X+1, Y}],
    ?assertEqual(AllowedDirections, filter_directions({X, Y}, PotentialDirections, HeightMap, CurrPath)).

print_matrix(M, DbgMessage, DbgLine) ->
    Str = lists:foldl(fun(Y, AccY) ->
                              Line = lists:foldl(fun(X, AccX) ->
                                                         %% ind(Key, Dict) -> {ok, Value} | error
                                                         Val = case dict:find({X, Y}, M#matrix.coords) of
                                                                   {ok, Value} ->
                                                                       %%?debugFmt("Val(~p, ~p): ~p", [X, Y, Value]),
                                                                       into_char(Value);
                                                                   error -> $.
                                                               end,
                                                         [Val|AccX]
                                                 end,
                                                 [],
                                                 lists:seq(1, M#matrix.max_x)),
                              [$\n|Line] ++ AccY
                end,
                [],
                lists:seq(1, M#matrix.max_y)),
    ?debugFmt("\nprint_matrix DbgMessage(~p): ~p\n" ++ lists:reverse(Str), [DbgLine, DbgMessage]).

%print_matrix_test() ->
%    {_, _, M} = load_input("test_input_day12.txt"),
%    print_matrix(M, "", ?LINE).

get_isolated(Point, M) ->
    M#matrix{coords=get_isolated(Point, M, dict:from_list([Point]))}.

get_isolated({{X, Y}, Val}, M, Acc) ->
    NewPoints = [{X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}], %% TODO: use get_adjacent_for_point()
    lists:foldl(fun({NewX, NewY}, OldAcc) ->
                        case dict:find({NewX, NewY}, OldAcc) of
                            {ok, _} -> OldAcc; %% Point already stored in accumulator
                            error ->
                                Found = dict:find({NewX, NewY}, M#matrix.coords),
                                case  Found of
                                    {ok, Val} -> get_isolated({{NewX, NewY}, Val}, M, dict:store({NewX, NewY}, Val, OldAcc));
                                    _ -> OldAcc
                                end
                        end
                end,
                Acc,
                NewPoints).


%% TODO: to much copy&paste below
matrix_subtract(#matrix{} = M1, #matrix{} = M2) ->
    XY1 = [{X,Y} || {{X,Y},_} <- dict:to_list(M1#matrix.coords)],
    XY2 = [{X,Y} || {{X,Y},_} <- dict:to_list(M2#matrix.coords)],
    NewXYSet = sets:subtract(
              sets:from_list(XY1),
              sets:from_list(XY2)),
    dict:from_list([{{X,Y},dict:fetch({X,Y},M1#matrix.coords)} || {X,Y} <- sets:to_list(NewXYSet)]).

points_subtract(P1, P2) ->
    XY1 = [{X,Y} || {{X,Y},_} <- dict:to_list(P1)],
    XY2 = [{X,Y} || {{X,Y},_} <- dict:to_list(P2)],
    NewXYSet = sets:subtract(
              sets:from_list(XY1),
              sets:from_list(XY2)),
    dict:from_list([{{X,Y},dict:fetch({X,Y},P1)} || {X,Y} <- sets:to_list(NewXYSet)]).

get_adjacent_for_point({X, Y}) -> [{X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}].

get_boundary(Point, M) ->
    Isolated = get_isolated(Point, M),
    PotentialAdjacentCords = dict:fold(fun(PointInIsolated, _Height, Acc) ->
                                               lists:foldl(fun(AdjPoint, AccForList) ->
                                                                   sets:add_element(AdjPoint, AccForList)
                                                           end,
                                                           Acc,
                                                           get_adjacent_for_point(PointInIsolated))
                                       end,
                                       sets:new(),
                                       Isolated#matrix.coords),

    IsolatedXY = sets:from_list([{X, Y} || {{X, Y}, _} <- dict:to_list(Isolated#matrix.coords)]),
    PotentialMinusIsolated = sets:to_list(sets:subtract(PotentialAdjacentCords, IsolatedXY)),
    %%?debugFmt("PotentialMinusIsolated: ~p", [PotentialMinusIsolated]),
    BoundryCoords = lists:foldl(fun({X, Y}, Acc) ->
                                        Found = dict:find({X, Y}, M#matrix.coords),
                                        case Found of
                                            {ok, Value} -> dict:store({X,Y}, Value, Acc);
                                            _ -> Acc
                                        end
                                end,
                                dict:new(),
                                PotentialMinusIsolated),
    M#matrix{coords=BoundryCoords}.

get_isolated_c_test() ->
    {_, _, M} = load_input("test_input_day12.txt"),
    IsolatedMatrix = get_isolated({{2,3}, 2}, M),
    %%print_matrix(IsolatedMatrix, "", ?LINE),
    ExpectedIsolatedArea = dict:from_list([{{3,2},2},{{2,3},2},{{3,3},2},{{2,4},2},{{3,4},2}]),
    ?assertEqual(ExpectedIsolatedArea, IsolatedMatrix#matrix.coords).

get_isolated_x_test() ->
    {_, _, M} = load_input("test_input_day12.txt"),
    IsolatedMatrix = get_isolated({{7,3},23}, M),
    %%print_matrix(IsolatedMatrix, "",  ?LINE),
    ExpectedIsolatedArea = dict:from_list([{{7,3},23},{{6,2},23},{{7,2},23}]),
    ?assertEqual(ExpectedIsolatedArea, IsolatedMatrix#matrix.coords).

get_boundary_test() ->
    {_, _, M} = load_input("test_input_day12.txt"),
    BoundaryMatrix = get_boundary({{2,3}, 2}, M),
    %%print_matrix(BoundaryMatrix, "",  ?LINE),
    ExpectedBoundaryArea = dict:from_list([{{3,1},1},
                                           {{2,2},1},{{4,2},17},
                                           {{1,3},0},{{4,3},18},
                                           {{1,4},0},{{4,4},19},
                                           {{2,5},1},{{3,5},3}]),
    ?assertEqual(ExpectedBoundaryArea, BoundaryMatrix#matrix.coords).

find_trap_area(_M, []) -> none;
find_trap_area(M, PotentialTrapPoints) ->
    {{_, _}, Level} = Point = hd(PotentialTrapPoints),
    PotentialTrapArea = get_isolated(Point, M),
    Boundary = get_boundary(Point, M),
    %% This is trap if eveything in Boundry is more than one level higher than Level of Point
    AreThereExits = lists:dropwhile(fun(BoundaryPoint) ->
                                                 {{_,_}, BoundaryPointLevel} = BoundaryPoint,
                                                 Result = (BoundaryPointLevel > Level + 1),
                                                 Result
                    end,
                    [BoundaryPoint || BoundaryPoint <- dict:to_list(Boundary#matrix.coords)]),
    NewPotentialTrapPoints = dict:to_list(points_subtract(dict:from_list(PotentialTrapPoints), PotentialTrapArea#matrix.coords)),
    case length(AreThereExits) > 0 of
        true ->
            find_trap_area(M, NewPotentialTrapPoints);
        false ->
            %% This is trap, return this area
            TrapArea = PotentialTrapArea,
            {TrapArea, NewPotentialTrapPoints}
    end.

remove_traps(M) ->
    LevelForTraps = 0,
    PotentialTrapPoints = [Point || Point = {{_,_}, V} <- dict:to_list(M#matrix.coords), V =:= LevelForTraps],
    remove_traps(M, PotentialTrapPoints).

remove_traps(M, PotentialTrapPoints) ->
    case find_trap_area(M, PotentialTrapPoints) of
        none ->
            M;
        {RealTrapM, NewPotentialTrapPoints} ->
            %%print_matrix(RealTrapM, "", ?LINE),
            NewM = M#matrix{coords=matrix_subtract(M, RealTrapM)},
            %%print_matrix(NewM, "", ?LINE),
            %%NewM
            remove_traps(NewM, NewPotentialTrapPoints)
    end.

remove_traps_test_() ->
    {timeout, 20, ?_test(begin
                             {_, _, InM} = load_input("input_day12.txt"),
                             M = remove_traps(InM),
                             %%print_matrix(M, "", ?LINE),
                             true
                         end)}.

matrix2points_list(#matrix{coords = Coords}) ->
    %%dict:to_list(dict:map(fun({X,Y}, _Value) -> {X,Y} end, Coords)).
    lists:map(fun({Point, Level}) -> Point end, dict:to_list(Coords)).

add_path(NewPath, []) ->
    [NewPath];
add_path(NewPath, Paths) ->
    NewPathLen = length(NewPath),
    CurPathLen = length(hd(Paths)),
    case NewPathLen =:= CurPathLen of
        true -> [NewPath|Paths];
        _ -> case NewPathLen > CurPathLen of
                 true -> Paths;
                 _ -> [NewPath]
             end
    end.

find_path_internal({X1, Y1}, {X2, Y2}, XYAllowedSet) ->
    [_, Result] = find_path_internal({X1, Y1}, {X2, Y2}, XYAllowedSet, [[], []]),
    lists:sort(Result).

find_path_internal({X, Y}, {X, Y}, _XYAllowedSet, [CurrPath, RestPaths]) ->
    ?debugFmt("Reached EndPoint!------------------------------------------------~n", []),
    [CurrPath, add_path(lists:reverse([{X,Y}|CurrPath]),RestPaths)];
find_path_internal({X, Y}, {EndX, EndY}, XYAllowedSet, [CurrPath, RestPaths]) ->
    ?debugFmt("find_path_internal head: X: ~p, Y: ~p    EndX: ~p, EndY: ~p~nCurrPath: ~p~nRestPaths: ~p~n", [X, Y, EndX, EndY, CurrPath, RestPaths]),

    AlreadyInPath = lists:member({X,Y}, CurrPath),
    case AlreadyInPath of
        true ->
            ?debugFmt("Path canceled1~n", []),
            %% Cancel this branch since {X,Y} is already in CurrPath
            [[CurrPath], RestPaths];
        _ ->
            CurrPathLen = length(CurrPath),
            StoredPathLen = case RestPaths of
                                [] -> CurrPathLen + 2; %% TODO: magic 2
                                _ -> length(hd(RestPaths))
                            end,
            case CurrPathLen + 1 > StoredPathLen of  %% TODO: magic 1
                true ->
                    ?debugFmt("Path canceled1~n", []),
                    %% Cancel this branch since its lenght is longer that lenth of the stored paths
                    [[CurrPath], RestPaths];
                _ ->
                    PotentialAdjPoints = get_adjacent_for_point({X,Y}),
                    ?debugFmt("PotentialAdjPoints: ~p~n", [PotentialAdjPoints]),
                    PotentialAdjPointsSet = sets:from_list(PotentialAdjPoints),

                    AdjPointsWithinAllowedSet = sets:intersection(PotentialAdjPointsSet, XYAllowedSet),
                    AdjPointsNotExistingInCurrPath = sets:subtract(AdjPointsWithinAllowedSet, sets:from_list(CurrPath)),

                    AdjPointsToCheck = sets:to_list(AdjPointsNotExistingInCurrPath),

                    case length(AdjPointsToCheck) > 0 of
                        true ->
                            ?debugFmt("AdjPointsToCheck: ~p~n", [AdjPointsToCheck]),
                            lists:foldl(fun({XNew,YNew}, [_Ignored, NewRestPaths]) ->
                                                find_path_internal({XNew,YNew}, {EndX,EndY}, XYAllowedSet, [[{X,Y}|CurrPath], NewRestPaths])
                                        end,
                                        [CurrPath, RestPaths],
                                        AdjPointsToCheck);
                        _ ->
                            ?debugFmt("DeadEnd path: ~p~n", [[{X,Y}|CurrPath]]),
                            [CurrPath, RestPaths]
                    end
            end
    end.

find_shortest_paths(BegPoint, EndPoint, M) ->
    BegPointLevel = dict:fetch(BegPoint, M#matrix.coords),
    MatrixPoints = matrix2points_list(M),
    MatrixPointsSet = sets:from_list(MatrixPoints),

    %% Filter chart points so only isolated area for BegPoint is left
    Isolated = get_isolated({BegPoint, BegPointLevel}, M),
    IsolatedPoints = matrix2points_list(Isolated),
    IsolatedPointsSet = sets:from_list(IsolatedPoints),

    %% Make sure EndPoint is adjacent to Isolated
    EndPointAdjacentSet = sets:from_list(get_adjacent_for_point(EndPoint)),
    Intersection = sets:intersection(EndPointAdjacentSet, IsolatedPointsSet),
    true = (sets:size(Intersection) > 0), %% Assertion that forces EndPoint to be at the edge os IsolatedPoints

    find_path_internal(BegPoint, EndPoint, sets:add_element(EndPoint, IsolatedPointsSet)).

    %% TODO: use real implementation
    %%lists:sort([{75,21}, {74,21}, {73,21}]).

find_paths_test_() ->
    BegPoint = {80,20},
    EndPoint = {75,21},
    ExpectedPaths = lists:sort([[{80,20}, {79,20}, {78,20}, {77,20}, {76,20}, {75,20}, {75,21}],
                                [{80,20}, {79,20}, {78,20}, {77,20}, {76,20}, {76,21}, {75,21}],
                                [{80,20}, {79,20}, {78,20}, {77,20}, {77,21}, {76,21}, {75,21}],
                                [{80,20}, {79,20}, {78,20}, {78,21}, {77,21}, {76,21}, {75,21}],
                                [{80,20}, {79,20}, {79,21}, {78,21}, {77,21}, {76,21}, {75,21}]
                               ]),

    {timeout, 20, ?_test(begin
                             %%{_, _, M} = load_input("input_day12_modified.txt"),
                             {_, _, M} = load_input("input_day12.txt"),
                             NewM = remove_traps(M),
                             ?assertEqual(ExpectedPaths, lists:sort(find_shortest_paths(BegPoint, EndPoint, NewM)))
                         end)}.

-endif.
