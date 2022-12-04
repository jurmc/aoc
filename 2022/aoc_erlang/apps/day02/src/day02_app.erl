-module(day02_app).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    InList = aoc_input_app:read_file_lines_into_2_columns(FileName),
    lists:sum(lists:map(fun(Item) -> round_outcome(Item) end, InList)).

part2(FileName) ->
    InList = requested_games(aoc_input_app:read_file_lines_into_2_columns(FileName)),
    lists:sum(lists:map(fun(Item) -> round_outcome(Item) end, InList)).

%%% Internal functions

%% TODO: from hardcoded sequences into +index = win, -index = loose
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

requested_game({HisCode, "Y"}) -> {HisCode, HisCode};
requested_game({HisCode = "A", "X"}) -> {HisCode, "C"};
requested_game({HisCode = "B", "X"}) -> {HisCode, "A"};
requested_game({HisCode = "C", "X"}) -> {HisCode, "B"};
requested_game({HisCode = "A", "Z"}) -> {HisCode, "B"};
requested_game({HisCode = "B", "Z"}) -> {HisCode, "C"};
requested_game({HisCode = "C", "Z"}) -> {HisCode, "A"}.

requested_games(InList) -> lists:map(fun requested_game/1, InList).

%%% Unit tests
-ifdef(TEST).

round_outcome_test() ->
    ?assertEqual(8, round_outcome({"A", "Y"})),
    ?assertEqual(1, round_outcome({"B", "X"})),
    ?assertEqual(6, round_outcome({"C", "Z"})).

requested_game_test() ->
    ?assertEqual({"A", "A"}, requested_game({"A", "Y"})),
    ?assertEqual({"B", "A"}, requested_game({"B", "X"})),
    ?assertEqual({"C", "A"}, requested_game({"C", "Z"})).

requested_games_test() ->
    ?assertEqual([{"A", "A"}, {"B", "A"}, {"C", "A"}], requested_games([{"A","Y"}, {"B", "X"}, {"C", "Z"}])).

part1_test() ->
    ?assertEqual(lists:sum([8, 1, 6]), part1("test_input_day02.txt")),
    ?assertEqual(15691, part1("input_day02.txt")).

part2_test() ->
    ?assertEqual(lists:sum([4, 1, 7]), part2("test_input_day02.txt")),
    ?assertEqual(12989, part2("input_day02.txt")).

-endif.
