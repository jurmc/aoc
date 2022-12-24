-module(day12_app).

%%-export([part1/1, part2/1]).
-compile(export_all).

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(UPPERCASE_S, 83).
-define(UPPERCASE_E, 69).
-define(LOWERCASE_A, 97).

%%% Exported functions

part2(_FileName) ->
    ok.

part1(FileName) ->
    {Begin, End, HeightMap} = load_input(FileName),
    %%?debugFmt("Begin: ~p, End: ~p~n", [Begin, End]),
    %%?debugFmt("HeightMap: ~n~p~n", [HeightMap]),
    Paths = get_paths(Begin, End, HeightMap),
    %%?debugFmt("Test paths: ~n~p~n", [Paths]),
    lists:min([length(Path) || Path <- Paths]) - 1.

%%% Internal functions

get_ends(CoordsList) ->
    {value, {Begin, _}} = lists:search(fun({{X, Y}, Val}) -> Val =:= ?UPPERCASE_S end, CoordsList),
    {value, {End, _}} = lists:search(fun({{X, Y}, Val}) -> Val =:= ?UPPERCASE_E end, CoordsList),
    {Begin, End}.

normalize_height(CoordsList) ->
    lists:map(fun({{X, Y}, Val}) ->
                      case Val of
                          ?UPPERCASE_S -> {{X, Y}, 0};
                          ?UPPERCASE_E -> {{X, Y}, 25};
                          _ -> {{X, Y}, (Val - ?LOWERCASE_A)}
                      end
              end,
              CoordsList).

load_input(FileName) ->
    Lines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]),
    LinesWithY = [{Y, lists:nth(Y, Lines)} || Y <- lists:seq(1, length(Lines))],
    CoordsList = lists:foldl(fun({Y, Line}, Acc) ->
                                     Acc ++ [{{X, Y}, lists:nth(X, Line)} || X <- lists:seq(1, length(Line))]
                             end,
                             [],
                             LinesWithY),
    {Begin, End} = get_ends(CoordsList),
    NormalizedCoordsList = normalize_height(CoordsList),
    {Begin, End, dict:from_list(NormalizedCoordsList)}.

begin_end_coords_test() ->
    {Begin, End, _} = load_input("test_input_day12.txt"),
    ?assertEqual({1, 1}, Begin),
    ?assertEqual({6, 3}, End).

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

get_paths(Begin, End, HeightMap) ->
    {_, Paths} = get_paths(Begin, End, HeightMap, {[Begin], []}, 0),
    Paths.

get_paths(BeginEqualToEnd, BeginEqualToEnd, _HeightMap, _Acc = {CurrPath, OtherPaths}, Depth) ->
    %%?debugFmt("get_paths clause1~n", []),
    {[], [lists:reverse(CurrPath)|OtherPaths]};
get_paths(Begin, End, _HeightMap, _Acc = {CurrPath, OtherPaths}, 2) ->
    %%?debugFmt("get_paths clause2~n", []),
    {[], [lists:reverse(CurrPath)|OtherPaths]};
get_paths(Begin = {X1, Y1}, End = {X2, Y2}, HeightMap, Acc = {CurrPath, _OtherPaths}, Depth) ->
    %%?debugFmt("Begin: ~p, End: ~p, Acc: ~p~n", [Begin, End, Acc]),
    PotentialDirections = [{X1+1, Y1},
                           {X1-1, Y1},
                           {X1, Y1+1},
                           {X1, Y1-1}],
    FilteredDirections = filter_directions(Begin, PotentialDirections, HeightMap, CurrPath), %% TODO: I bet this part will have to be adapted somehow for part2 :)
    lists:foldl(fun(NewPositon, {_, OtherPaths}) ->
                        %%?debugFmt("NewPositon: ~p, End: ~p, CurrPath: ~p, OtherPaths: ~p, Depth: ~p", [NewPositon, End, CurrPath, OtherPaths, Depth]),
                        %%Out = get_paths(NewPositon, End, HeightMap, {[NewPositon|CurrPath], OtherPaths}, Depth+1),
                        Out = get_paths(NewPositon, End, HeightMap, {[NewPositon|CurrPath], OtherPaths}, Depth),
                        %%?debugFmt("Out: ~p~n", [Out]),
                        Out
                end,
                Acc,
                FilteredDirections).

%%get_paths_test() ->
%%    Begin = {1,1},
%%    End = {2,2},
%%    HeightMap = dict:from_list([{{1,1}, 0},
%%                             {{2,1}, 1},
%%                             {{1,2}, 2},
%%                             {{2,2}, 2}]),
%%    ExpectedPaths = [[{1,1}, {2,1}, {2,2}]],
%%    Paths = get_paths(Begin, End, HeightMap),
%%    ?debugFmt("Paths: ~n~p~n", [Paths]),
%%    ?assertEqual(ExpectedPaths, Paths).

part1_test() ->
    Result = part1("test_input_day12.txt"),
    ?debugFmt("Result: ~p~n", [length(List) || List <- Result]).

-endif.
