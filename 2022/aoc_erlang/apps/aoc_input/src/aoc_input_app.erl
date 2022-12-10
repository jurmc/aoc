-module(aoc_input_app).

-export([read_file/1, read_file_lines/1, read_file_lines/2, read_file_lines2/1, read_file_lines_into_2_columns/1, remove_empty_lines/1]).

%%%
%%% Reading files
%%%

read_file(FileName) ->
    file:read_file(os:getenv("AOC_INPUT") ++ "/" ++ FileName).


read_file_lines(FileName) ->
    {ok, FileContent} = read_file(FileName),
    string:tokens(erlang:binary_to_list(FileContent), "\n").

% TODO: for now only legal option is: split_lines_into_words
read_file_lines(FileName, [Opts]) ->
    InLines = read_file_lines(FileName),

    OutLines = case Opts of
                   split_lines_into_words ->
                       lists:map(fun(Line) -> string:tokens(Line, " ") end, InLines);
                   _ -> InLines
               end,
    OutLines.

%% TODO: Merge it with read_file_lines (above)
read_file_lines2(FileName) ->
    {ok, FileContent} = read_file(FileName),
    file_content_into_list_of_lines(FileContent).

%% TODO: Somethin more generic?
read_file_lines_into_2_columns(FileName) ->
    into_2_columns(read_file_lines(FileName)).

%%%
%%% Conversions
%%%

%% TODO: Later this might be parametrized
%% TODO: or sucha converstions will become internal and read_file_lines/2 have to be used with proper Opts
into_2_columns(Lines) ->
    lists:map(fun(Line) -> [I1, I2] = string:tokens(Line, " "), {I1, I2} end, Lines).

remove_empty_lines(Lines) ->
    lists:filter(fun(X) -> [] =/= X end, Lines).

%%%
%%% Internal functions
%%%

file_content_into_list_of_lines(FileContent) ->
    %%[[], "ab", [], "cd", "ef"].
    file_content_into_list_of_lines(FileContent, []).

file_content_into_list_of_lines([], Acc) -> lists:reverse(Acc);
file_content_into_list_of_lines([10|T], Acc) ->
    file_content_into_list_of_lines(T, [[]|Acc]);
file_content_into_list_of_lines(FileContent, Acc) ->
    case string:split(FileContent, "\n") of
        [Line, RestLines] -> file_content_into_list_of_lines(RestLines, [Line|Acc]);
        [Line] -> file_content_into_list_of_lines([], [Line|Acc])
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

file_content_into_list_of_lines_test() ->
    ?assertEqual([[], "ab", [], "cd", "ef"], file_content_into_list_of_lines("\nab\n\ncd\nef\n")),
    ?assertEqual(["ab", [], "cd", "ef", []], file_content_into_list_of_lines("ab\n\ncd\nef\n\n")).

remove_empty_lines_test() ->
    ?assertEqual(["ab", "cd", "ef"], remove_empty_lines([[], "ab", [], "cd", "ef"])),
    ?assertEqual(["ab", "cd", "ef"], remove_empty_lines(["ab", [], "cd", "ef", []])).

-endif.

