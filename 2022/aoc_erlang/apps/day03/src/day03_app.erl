-module(day03_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    sum_priorities(aoc_input_app:read_file_lines(FileName)).

part2(FileName) ->
    InList = aoc_input_app:read_file_lines(FileName),
    Groups = group_input(InList),
    sum_priorities_for_part2(Groups).

%%% Internal functions

sum_priorities(InList) ->
    lists:sum(priorites_list(InList)).

priorites_list(InList) ->
    lists:map(fun(Line) -> item_to_priority(shared_item(Line)) end, InList).

shared_item(String) ->
    [L1, L2] = split_in_half(String),
    S1 = sets:from_list(L1),
    S2 = sets:from_list(L2),
    Result = sets:to_list(sets:intersection(S1, S2)),
    _AssumeOneElem = 1 = string:length(Result),
    Result.

item_to_priority(Char) when Char >= "a" -> hd(Char) - 96;
item_to_priority(Char) -> hd(Char) - 38.

split_in_half(String) ->
    Len = string:length(String),
    _AssumeLenIsEven = 0 = Len rem 2,
    Pos = Len div 2,
    [string:slice(String, 0, Pos), string:slice(String, Pos)].

group_input(InList) ->
    lists:reverse(group_input(InList, [])).

group_input([], Acc) -> Acc;
group_input([A,B,C|T], Acc) ->
    group_input(T, [{A,B,C}|Acc]).

%% TODO: foldl candidate (also previous days can be revised)
sum_priorities_for_part2(Groups) ->
    lists:sum(lists:map(fun(Group) -> priority_for_group(Group) end, Groups)).

%% TODO: foldl candidate
priority_for_group(Group) ->
    {L1, L2, L3} = Group,
    S1 = sets:from_list(L1),
    S2 = sets:from_list(L2),
    S3 = sets:from_list(L3),
    S4 = sets:intersection(S1, S2),
    S5 = sets:intersection(S3, S4),
    item_to_priority(sets:to_list(S5)).

%%% Unit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_in_half_test() ->
    ?assertEqual(["vJrwpWtwJgWr", "hcsFMMfFFhFp"], split_in_half("vJrwpWtwJgWrhcsFMMfFFhFp")),
    ?assertEqual(["jqHRNqRjqzjGDLGL", "rsFMfFZSrLrFZsSL"], split_in_half("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")),
    ?assertEqual(["PmmdzqPrV", "vPwwTWBwg"], split_in_half("PmmdzqPrVvPwwTWBwg")).

shared_item_test() ->
    ?assertEqual("p", shared_item("vJrwpWtwJgWrhcsFMMfFFhFp")),
    ?assertEqual("L", shared_item("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")),
    ?assertEqual("P", shared_item("PmmdzqPrVvPwwTWBwg")).

item_to_priority_test() ->
    ?assertEqual(1, item_to_priority("a")),
    ?assertEqual(16, item_to_priority("p")),
    ?assertEqual(38, item_to_priority("L")),
    ?assertEqual(42, item_to_priority("P")).

priorites_test() ->
    InList = aoc_input_app:read_file_lines("test_input_day03.txt"),
    ?assertEqual([16, 38, 42, 22, 20, 19], priorites_list(InList)).


group_input_test() ->
    InList = aoc_input_app:read_file_lines("test_input_day03.txt"),
    ?assertEqual([{"vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"},
                  {"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"}],
                 group_input(InList)).

common_char_for_group_test() ->
    InList = aoc_input_app:read_file_lines("test_input_day03.txt"),
    Groups = group_input(InList),
    ?assertEqual(18, priority_for_group(lists:nth(1, Groups))),
    ?assertEqual(52, priority_for_group(lists:nth(2, Groups))).

part1_test() ->
    ?assertEqual(157, part1("test_input_day03.txt")),
    ?assertEqual(7716, part1("input_day03.txt")).

part2_test() ->
    ?assertEqual(70, part2("test_input_day03.txt")),
    ?assertEqual(2973, part2("input_day03.txt")).

-endif.
