-module(day13_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    Input = read_input_into_pairs(FileName),
    Indices = lists:filter(fun(Idx) ->
                                  {L1,L2} = lists:nth(Idx, Input),
                                  compare(L1,L2) =:= -1
                          end,
                          lists:seq(1,length(Input))),
    SumIndices = lists:sum(Indices),
    SumIndices.

part2(FileName) ->
    DivPacket1 = [[2]],
    DivPacket2 = [[6]],
    Input = [DivPacket1,DivPacket2] ++ read_input(FileName),
    SortedInput = lists:sort(fun less_than_or_equal/2, Input),
    Idx1 = 1 + length(lists:takewhile(fun(Item) -> Item =/= DivPacket1 end, SortedInput)),
    Idx2 = 1 + length(lists:takewhile(fun(Item) -> Item =/= DivPacket2 end, SortedInput)),
    Idx1 * Idx2.

%%% Internal functions

read_input(FileName) ->
    InLines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]),
    lists:map(fun(Line) ->
                      read_list(Line)
              end,
              InLines).

read_input_into_pairs(FileName) ->
    InLists = read_input(FileName),
    lists:map(fun(Idx) ->
                      {lists:nth(Idx, InLists),
                       lists:nth(Idx+1, InLists)}
              end,
              lists:seq(1, length(InLists), 2)).

compare([], []) -> 0;
compare([], _) -> -1;
compare(_, []) -> 1;
compare([H1|T1], [H2|T2]) when is_list(H1), is_integer(H2) ->
    case compare(H1, [H2]) of
        0 -> compare(T1,T2);
        RetVal -> RetVal
    end;
compare([H1|T1], [H2|T2]) when is_list(H2), is_integer(H1) ->
    case compare([H1], H2) of
        0 -> compare(T1,T2);
        RetVal -> RetVal
    end;
compare([H1|T1], [H2|T2]) when is_list(H1), is_list(H2) ->
    case compare(H1,H2) of
        0 -> compare(T1,T2);
        RetVal -> RetVal
    end;
compare([H1|T1], [H2|T2]) when is_integer(H1), is_integer(H2) ->
    case H1 =:= H2 of
       true -> compare(T1,T2);
        _ -> case H1 < H2 of
                 true -> -1;
                 false -> 1
             end
    end.

less_than_or_equal(L1,L2) ->
    case compare(L1,L2) of
        1 -> false;
        _ -> true
    end.

read_list(Line) ->
    {_RestLine, Items} = read_list(Line, {[], []}),
    lists:nth(1,Items).

%% {RestLine, Items} = read_list(Line, {Items = [], OngointItem = []}).
read_list([], {Items, []}) ->
    {[], Items};
read_list([$]|T], {Items, []}) ->
    {T, lists:reverse(Items)};
read_list([$]|T], {Items, OngoingDigits}) ->
    Int = to_integer(lists:reverse(OngoingDigits)),
    {T, lists:reverse([Int|Items])};
read_list([$[|T], {Items, _OngoingDigits = ""}) ->
    {NewT, NewItems} = read_list(T, {[], ""}),
    read_list(NewT, {[NewItems|Items], ""});
read_list([$,|T], {Items, ""}) ->
    read_list(T, {Items, ""});
read_list([$,|T], {Items, OngoingDigits}) ->
    Int = to_integer(lists:reverse(OngoingDigits)),
    read_list(T, {[Int|Items], []});
read_list([H|T], {Items, OngoingDigits}) ->
    read_list(T, {Items, [H|OngoingDigits]}).

to_integer(Str) ->
    {Int, []} = string:to_integer(Str),
    Int.

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_list_test() ->
    Line1 = "[1,1,3,1,1]",
    ?assertEqual([1,1,3,1,1], read_list(Line1)),

    Line2 = "[[1,2],[3,4,5]]",
    ?assertEqual([[1,2],[3,4,5]], read_list(Line2)),

    Line3 = "[[4,4],4,4]",
    ?assertEqual([[4,4],4,4], read_list(Line3)),

    Line4 = "[[[]]]",
    ?assertEqual([[[]]], read_list(Line4)),

    Line5 = "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    ?assertEqual([1,[2,[3,[4,[5,6,7]]]],8,9], read_list(Line5)).

compare_lists_test() ->
    ?assertEqual(0, compare([1,2,3], [1,2,3])),
    ?assertEqual(-1, compare([1,2,3], [1,2,4])),
    ?assertEqual(1, compare([1,2,3], [0,2,3])),

    ?assertEqual(-1, compare([1,2], [1,2,3])),
    ?assertEqual(1, compare([1,2,3], [1,2])),

    ?assertEqual(0, compare([1,[2],3], [1,2,3])),
    ?assertEqual(-1, compare([1,[1],3], [1,2,3])),
    ?assertEqual(1, compare([1,2,3], [1,2,[2]])),

    ?assertEqual(-1, compare([[1],[2,3,4]], [[1],4])),
    ?assertEqual(-1, compare([], [7,[],10,6,[]])).

part1_test() ->
    ?assertEqual(13, part1("test_input_day13.txt")),
    ?assertEqual(5684, part1("input_day13.txt")).

part2_test() ->
    ?assertEqual(140, part2("test_input_day13.txt")),
    ?assertEqual(22932, part2("input_day13.txt")).

-endif.
