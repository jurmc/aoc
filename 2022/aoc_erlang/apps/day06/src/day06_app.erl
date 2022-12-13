-module(day06_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    {ok, FileContent} = aoc_input_app:read_file(FileName),
    String = binary:bin_to_list(FileContent),
    detect_marker(String, 4).

part2(FileName) ->
    {ok, FileContent} = aoc_input_app:read_file(FileName),
    String = binary:bin_to_list(FileContent),
    detect_marker(String, 14).

%%% Internal functions

detect_marker(String, Length) -> detect_marker(String, Length, Length).

detect_marker(String, Length, Acc) ->
    {AnalyzedString, Rest} = lists:split(Length, String),
    case sets:size(sets:from_list(AnalyzedString)) of
        Length -> {AnalyzedString, Acc};
        _ -> detect_marker(tl(AnalyzedString) ++ Rest, Length, Acc+1)
    end.

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

detect_marker_test() ->
    Part1MarkerLen = 4,
    ?assertEqual({"jpqm", 7} , detect_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", Part1MarkerLen)),
    ?assertEqual({"vwbj", 5}, detect_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", Part1MarkerLen)),
    ?assertEqual({"pdvj", 6}, detect_marker("nppdvjthqldpwncqszvftbrmjlhg", Part1MarkerLen)),
    ?assertEqual({"rfnt", 10}, detect_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", Part1MarkerLen)),
    ?assertEqual({"zqfr", 11}, detect_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", Part1MarkerLen)),

    Part2MarkerLen = 14,
    ?assertEqual(19, element(2, detect_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", Part2MarkerLen))),
    ?assertEqual(23, element(2, detect_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", Part2MarkerLen))),
    ?assertEqual(23, element(2, detect_marker("nppdvjthqldpwncqszvftbrmjlhg", Part2MarkerLen))),
    ?assertEqual(29, element(2, detect_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", Part2MarkerLen))),
    ?assertEqual(26, element(2, detect_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", Part2MarkerLen))).

part1_test() ->
    {_, Position} = part1("input_day06.txt"),
    ?assertEqual(1480, Position).

part2_test() ->
    {_, Position} = part2("input_day06.txt"),
    ?assertEqual(2746, Position).

-endif.
