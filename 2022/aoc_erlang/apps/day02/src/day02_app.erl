-module(day02_app).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([]).

%%% Exported functions

%%% Internal functions

%% X: lose,
%% Y: draw
%% Z: win

into_shape("A") -> rock;
into_shape("X") -> rock;
into_shape("B") -> paper;
into_shape("Y") -> paper;
into_shape("C") -> scissors;
into_shape("Z") -> scissors.

round_outcome({HisCode, OwnCode}) ->
    HisShape = into_shape(HisCode),
    OwnShape = into_shape(OwnCode),
    ResultPoints = result_points(result(HisShape, OwnShape)),
    ShapePoints = shape_points(OwnShape),
    ResultPoints + ShapePoints.

result(TheSame, TheSame) -> draw;
result(rock, paper) -> won;
result(rock, scissors) -> lost;
result(paper, scissors) -> won;
result(paper, rock ) -> lost;
result(scissors, rock ) -> won;
result(scissors, paper ) -> lost.

result_points(won) -> 6;
result_points(draw) -> 3;
result_points(lost) -> 0.

shape_points(rock) -> 1;
shape_points(paper) -> 2;
shape_points(scissors) -> 3.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


round_outcome_test() ->
    ?assertEqual(8, round_outcome({"A", "Y"})),
    ?assertEqual(1, round_outcome({"B", "X"})),
    ?assertEqual(6, round_outcome({"C", "Z"})).

part1(InList) ->
    lists:sum(lists:map(fun(Item) -> round_outcome(Item) end, InList)).


into_list(FileContent) ->
    Lines = string:tokens(erlang:binary_to_list(FileContent), "\n"),
    lists:map(fun(Line) -> [I1, I2] = string:tokens(Line, " "), {I1, I2} end, Lines).


test_input_test() ->
    TestInput = [{"A", "Y"}, {"B", "X"}, {"C", "Z"}],
    ?assertEqual(lists:sum([8, 1, 6]), part1(TestInput)),

    {ok, FileContent} = aoc_input_app:read_file("test_input_day02.txt"),
    ?assertEqual(<<"A Y\nB X\nC Z\n">>, FileContent),
    ?assertEqual([{"A", "Y"}, {"B", "X"}, {"C", "Z"}], into_list(FileContent)),
    ?assertEqual(lists:sum([8, 1, 6]), part1(into_list(FileContent))).

normal_input_test() ->
    {ok, FileContent} = aoc_input_app:read_file("input_day02.txt"),
    Result = part1(into_list(FileContent)),
    ?debugFmt("Result: ~p", [Result]).

requested_game({HisCode, "Y"}) -> {HisCode, HisCode};
requested_game({HisCode = "A", "X"}) -> {HisCode, "C"};
requested_game({HisCode = "B", "X"}) -> {HisCode, "A"};
requested_game({HisCode = "C", "X"}) -> {HisCode, "B"};
requested_game({HisCode = "A", "Z"}) -> {HisCode, "B"};
requested_game({HisCode = "B", "Z"}) -> {HisCode, "C"};
requested_game({HisCode = "C", "Z"}) -> {HisCode, "A"}.

requested_games(InList) ->
    lists:map(fun requested_game/1, InList).

requested_game_test() ->
    ?assertEqual({"A", "A"}, requested_game({"A", "Y"})),
    ?assertEqual({"B", "A"}, requested_game({"B", "X"})),
    ?assertEqual({"C", "A"}, requested_game({"C", "Z"})).

requested_games_test() ->
    {ok, FileContent} = aoc_input_app:read_file("test_input_day02.txt"),
    ?assertEqual([{"A", "A"}, {"B", "A"}, {"C", "A"}], requested_games(into_list(FileContent))),
    ?assertEqual(lists:sum([4, 1, 7]), part1(requested_games(into_list(FileContent)))).

real_input_requested_games_test() ->
    {ok, FileContent} = aoc_input_app:read_file("input_day02.txt"),
    Result = part1(requested_games(into_list(FileContent))),
    ?debugFmt("Result: ~p", [Result]).

-endif.
