-module(aoc_input_app).

-export([read_file/1, read_file_lines/1, read_file_lines_into_2_columns/1]).

%%% Reading files

read_file(FileName) ->
    file:read_file(os:getenv("AOC_INPUT") ++ "/" ++ FileName).

read_file_lines(FileName) ->
    {ok, FileContent} = read_file(FileName),
    string:tokens(erlang:binary_to_list(FileContent), "\n").

read_file_lines_into_2_columns(FileName) ->
    into_2_columns(read_file_lines(FileName)).

%%% Conversions

%% TODO: Later this might be parametrized
into_2_columns(Lines) ->
    lists:map(fun(Line) -> [I1, I2] = string:tokens(Line, " "), {I1, I2} end, Lines).

