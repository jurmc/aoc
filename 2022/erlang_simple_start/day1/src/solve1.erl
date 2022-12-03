-module(solve1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([input_into_list/1, part1/1, part2/1]).

%%
%% Exported functions
%%

part1(FileName) ->
    {ok, FileContent} = file:read_file(FileName),
    lists:max(get_sum_for_each_elve(FileContent)).

part2(FileName) ->
    {ok, FileContent} = file:read_file(FileName),
    [First, Second, Third | _] = lists:reverse(lists:sort(get_sum_for_each_elve(FileContent))),
    lists:sum([First, Second, Third]).

%%
%% Internal functions
%%

reverse(Bytes) ->
    reverse(Bytes, <<>>).

reverse(<<>>, Out) -> Out;
reverse(<<F,Rest/binary>>, <<Out/binary>>) -> reverse(<<Rest/binary>>, <<F,Out/binary>>).

input_into_list(FileContent) -> input_into_list(FileContent, []).

input_into_list(<<>>, Acc) ->
    lists:reverse(lists:map(fun([]) -> [];
                               (<<Bytes/binary>>) -> reverse(Bytes) end,
                            Acc));

input_into_list(<<F,R/binary>>, Acc) when F == 10 -> input_into_list(R, [[]|Acc]);
input_into_list(<<F,R/binary>>, [[]|T]) -> input_into_list(R, [<<F>>,[]|T]);
input_into_list(<<F,R/binary>>, [H|T]) -> input_into_list(R, [<<F,H/binary>>|T]);
input_into_list(<<F,R/binary>>, []) -> input_into_list(R, [<<F>>]).

convert_to_int(InList) ->
    lists:map(fun([]) -> [];
                 (Item) -> {Int, _} = string:to_integer(binary_to_list(Item)), Int end,
              InList).

get_sum_for_each_elve(FileContent) ->
        ConvertedToInt = convert_to_int(input_into_list(FileContent)),
        get_sum_for_each_elve(ConvertedToInt, [0]).

get_sum_for_each_elve([], Acc) -> lists:reverse(Acc);
get_sum_for_each_elve([[],[]|T], Acc) -> get_sum_for_each_elve(T, [0|Acc]);
get_sum_for_each_elve([[]|T], Acc) -> get_sum_for_each_elve(T, Acc);
get_sum_for_each_elve([H|T], [AccH]) -> get_sum_for_each_elve(T, [H+AccH]);
get_sum_for_each_elve([H|T], [AccH|AccT]) -> get_sum_for_each_elve(T, [H+AccH|AccT]).

%%
%% Unit tests
%%
-ifdef(TEST).

input_into_list_test() ->
    ?assertEqual([[], [], <<"123">>, [], <<"4567">>, [], <<"89">>, []], input_into_list(<<"\n\n123\n4567\n89\n">>)).

convert_to_int_test() ->
    ?assertEqual([[], [], 123, [], 4567, [], 89, []], convert_to_int([[], [], <<"123">>, [], <<"4567">>, [], <<"89">>, []])).

get_sum_for_each_elve_test() ->
    ExpectedSumForEachElve = [6000, 4000],
    ?assertEqual(ExpectedSumForEachElve, get_sum_for_each_elve([4000, [], 2000, [], [], 1000, 3000, []], [0])).

part1_test() ->
    {ok, TestFileContent} = file:read_file("test_input_day1.txt"),
    ?assertEqual(24000, part1("test_input_day1.txt")),

    {ok, FileContent} = file:read_file("input_day1.txt"),
    ?assertEqual(74394, part1("input_day1.txt")).

part2_test() ->
    {ok, TestFileContent} = file:read_file("test_input_day1.txt"),
    ?assertEqual(lists:sum([24000,11000,10000]), part2("test_input_day1.txt")),

    {ok, FileContent} = file:read_file("input_day1.txt"),
    ?assertEqual(lists:sum([74394,69863,68579]), part2("input_day1.txt")).

-endif.
