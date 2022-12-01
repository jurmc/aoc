-module(solve1).

-include_lib("eunit/include/eunit.hrl").

-export([input_into_list/1]).

reverse(Bytes) ->
    reverse(Bytes, <<>>).

reverse(<<>>, Out) ->
    Out;
reverse(<<F,Rest/binary>>, <<Out/binary>>) ->
    reverse(<<Rest/binary>>, <<F,Out/binary>>).

input_into_list(FileContent) ->
    input_into_list(FileContent, []).

input_into_list(<<>>, Acc) ->
    lists:reverse(lists:map(fun([]) -> [];
                               (<<Bytes/binary>>) -> reverse(Bytes) end,
                            Acc));

input_into_list(<<F,R/binary>>, Acc) when F == 10 ->
    input_into_list(R, [[]|Acc]);
input_into_list(<<F,R/binary>>, [[]|T]) ->
    input_into_list(R, [<<F>>,[]|T]);
input_into_list(<<F,R/binary>>, [H|T]) ->
    input_into_list(R, [<<F,H/binary>>|T]);
input_into_list(<<F,R/binary>>, []) ->
    input_into_list(R, [<<F>>]).


solve_test() ->
    ?assertEqual([[], [], <<"123">>, [], <<"4567">>, [], <<"89">>, []], input_into_list(<<"\n\n123\n4567\n89\n">>)),
    {ok, FileContent} = file:read_file("test_input_day1.txt"),
    input_into_list(FileContent).


