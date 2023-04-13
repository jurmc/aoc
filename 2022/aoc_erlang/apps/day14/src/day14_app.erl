-module(day14_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(_FileName) ->
    ok.

part2(_FileName) ->
    ok.

%%% Internal functions
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_input(FileName) ->
    InLines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]).
    %%lists:map(fun(Line) ->
    %%                  re:split()
    %%          end,
    %%          InLines).


%%% Unit tests

first_test() ->
    ?assertEqual(ok, ok),
    Input = read_input("test_input_day14.txt"),
    ?debugFmt("Input: ~p~n", [Input]).

-endif.
