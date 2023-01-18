-module(day13_app).

-export([part1/1, part2/1]).

%%% Exported functions

part2(_FileName) ->
    ok.

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_input(FileName) ->
    InLines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]),
    Pairs = lists:map(fun(Idx) ->
                              {read_list(lists:nth(Idx, InLines)),
                               read_list(lists:nth(Idx+1, InLines))}
                      end,
                      lists:seq(1, length(InLines), 2)).

to_integer(Str) ->
    %%?debugFmt("Str: ~p", [Str]),
    {Int, []} = string:to_integer(Str),
    Int.

read_list(Line) ->
    {_RestLine, Items} = read_list(Line, {[], []}),
    %%?debugFmt("Result Items: ~p", [Items]),
    lists:nth(1,Items).

%% {RestLine, Items} = read_list(Line, {Items = [], OngointItem = []}).
read_list([], {Items, []}) ->               % ]     - goStackUp
    %%?debugFmt("Final: Items: ~p", [Items]),
    {[], Items};
read_list([$]|T], {Items, []}) ->
    %%?debugFmt("Mark1.1(])", []),
    {T, lists:reverse(Items)};
read_list([$]|T], {Items, OngoingDigits}) ->
    %%?debugFmt("Mark1.2(]): Items: ~p, OngoingDigits: ~p", [Items, OngoingDigits]),
    Int = to_integer(lists:reverse(OngoingDigits)),
    {T, lists:reverse([Int|Items])};
read_list([$[|T], {Items, OngoingDigits = ""}) -> % [     - goStackDown
    %%?debugFmt("Mark2.1([): Items: ~p", [Items]),
    {NewT, NewItems} = read_list(T, {[], ""}),
    %%?debugFmt("Mark2.2([):   NewT: ~p, NewItems: ~p", [NewT, NewItems]),
    read_list(NewT, {[NewItems|Items], ""});
read_list([$,|T], {Items, ""}) ->      % ,     - closeItem
    %%?debugFmt("Mark3.1(,): Items: ~p", [Items]),
    read_list(T, {Items, ""});
read_list([$,|T], {Items, OngoingDigits}) ->      % ,     - closeItem
    %%?debugFmt("Mark3.2(,): Items: ~p OngoingDigits: ~p", [Items, OngoingDigits]),
    Int = to_integer(lists:reverse(OngoingDigits)),
    read_list(T, {[Int|Items], []});
read_list([H|T], {Items, OngoingDigits}) ->       % digit - buildUpItem
    %%?debugFmt("Mark4(digit)", []),
    read_list(T, {Items, [H|OngoingDigits]}).

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

compare_lists([], []) ->
    0;
compare_lists([], [_]) ->
    -1;
compare_lists([_], []) ->
    1;
compare_lists([H1|T1], [H2|T2]) when is_list(H1), is_integer(H2) ->
    case compare_lists(H1, [H2]) of
        0 -> compare_lists(T1,T2);
        RetVal -> RetVal
    end;
compare_lists([H1|T1], [H2|T2]) when is_list(H2), is_integer(H1) ->
    case compare_lists([H1], H2) of
        0 -> compare_lists(T1,T2);
        RetVal -> RetVal
    end;
compare_lists([H1|T1], [H2|T2]) when is_list(H1), is_list(H2) ->
    case compare_lists(H1,H2) of
        0 -> compare_lists(T1,T2);
        RetVal -> RetVal
    end;
compare_lists([H1|T1], [H2|T2]) when is_integer(H1), is_integer(H2) ->
    case H1 =:= H2 of
       true -> compare_lists(T1,T2);
        _ -> case H1 < H2 of
                 true -> -1;
                 false -> 1
             end
    end.

compare_lists_test() ->
    ?assertEqual(0, compare_lists([1,2,3], [1,2,3])),
    ?assertEqual(-1, compare_lists([1,2,3], [1,2,4])),
    ?assertEqual(1, compare_lists([1,2,3], [0,2,3])),

    ?assertEqual(-1, compare_lists([1,2], [1,2,3])),
    ?assertEqual(1, compare_lists([1,2,3], [1,2])),

    ?assertEqual(0, compare_lists([1,[2],3], [1,2,3])),
    ?assertEqual(-1, compare_lists([1,[1],3], [1,2,3])),
    ?assertEqual(1, compare_lists([1,2,3], [1,2,[2]])),

    ?assertEqual(-1, compare_lists([[1],[2,3,4]], [[1],4])).

part1(FileName) ->
    Input = read_input(FileName),
    ?debugFmt("Input: ~p", [Input]),
    Indices = lists:filter(fun(Idx) ->
                                  {L1,L2} = lists:nth(Idx, Input),
                                  compare_lists(L1,L2) =:= -1 
                          end,
                          lists:seq(1,length(Input))),
    ?debugFmt("Indices: ~p", [Indices]),
    SumIndices = lists:sum(Indices),
    ?debugFmt("SumIndices: ~p", [SumIndices]),
    SumIndices.


part1_test() ->
    ?assertEqual(13, part1("test_input_day13.txt")),
    Result = part1("input_day13.txt"),
    ?debugFmt("Result: ~p", [Result]).

-endif.
