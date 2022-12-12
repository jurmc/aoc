-module(day08_app).

%%-export([part1/1, part2/1]).

%%% Exported functions

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_input(FileName) ->
    aoc_input_app:read_file_lines(FileName, [remove_line_breaks, split_lines_into_characters]).

init_test() ->
    Input = read_input("test_input_day08.txt"),
    ?assertEqual(ok, Input).

-endif.
