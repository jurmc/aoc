-module(aoc_input_app).

-export([read_file/1]).

read_file(FileName) ->
    file:read_file(os:getenv("AOC_INPUT") ++ "/" ++ FileName).
