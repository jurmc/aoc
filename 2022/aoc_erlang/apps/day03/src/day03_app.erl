-module(day03_app).

-include_lib("eunit/include/eunit.hrl").

split_in_half(String) ->
    Len = string:length(String),
    ?assertEqual(0, Len rem 2),
    Pos = Len div 2,
    [string:slice(String, 0, Pos), string:slice(String, Pos)].

shared_item(String) ->
    [L1, L2] = split_in_half(String),
    S1 = sets:from_list(L1),
    S2 = sets:from_list(L2),
    Result = sets:to_list(sets:intersection(S1, S2)),
    ?assertEqual(1, string:length(Result)),
    Result.

split_in_half_test() ->
    ?assertEqual(["vJrwpWtwJgWr", "hcsFMMfFFhFp"], split_in_half("vJrwpWtwJgWrhcsFMMfFFhFp")),
    ?assertEqual(["jqHRNqRjqzjGDLGL", "rsFMfFZSrLrFZsSL"], split_in_half("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")),
    ?assertEqual(["PmmdzqPrV", "vPwwTWBwg"], split_in_half("PmmdzqPrVvPwwTWBwg")).

shared_item_test() ->
    ?assertEqual("p", shared_item("vJrwpWtwJgWrhcsFMMfFFhFp")),
    ?assertEqual("L", shared_item("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")),
    ?assertEqual("P", shared_item("PmmdzqPrVvPwwTWBwg")).


%%    Lowercase item types a through z have priorities 1 through 26.
%%    Uppercase item types A through Z have priorities 27 through 52.
item_to_priority(Char) when Char >= "a" -> hd(Char) - 96;
item_to_priority(Char) -> hd(Char) - 38.

item_to_priority_test() ->
    ?assertEqual(1, item_to_priority("a")),
    ?assertEqual(16, item_to_priority("p")),
    ?assertEqual(38, item_to_priority("L")),
    ?assertEqual(42, item_to_priority("P")).

priorites_list(InList) ->
    %%[16, 38, 42, 22, 20, 19].
    lists:map(fun(Line) -> item_to_priority(shared_item(Line)) end, InList).


priorites_test() ->
    InList = aoc_input_app:read_file_lines("test_input_day03.txt"),
    ?assertEqual([16, 38, 42, 22, 20, 19], priorites_list(InList)).

sum_priorities(InList) ->
    lists:sum(priorites_list(InList)).

sum_priorities_test() ->
    InListTest = aoc_input_app:read_file_lines("test_input_day03.txt"),
    ?assertEqual(157, sum_priorities(InListTest)),

    InList = aoc_input_app:read_file_lines("input_day03.txt"),
    Result = sum_priorities(InList),
    ?debugFmt("Result: ~p", [Result]).
    %%?assertEqual(157, Result).


