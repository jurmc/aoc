-module(aoc_input_app).

-define(NEWLINE, 10).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([read_file/1, read_file_lines/1, read_file_lines/2, read_file_lines_do_not_remove_line_breaks/1, read_file_lines2/1, read_file_lines_into_2_columns/1, remove_empty_lines/1]).

%%%
%%% Reading files
%%%

read_file(FileName) ->
    file:read_file(os:getenv("AOC_INPUT") ++ "/" ++ FileName).

read_file_lines_do_not_remove_line_breaks(FileName) ->
    {ok, FileContent} = read_file(FileName),
    read_file_lines_do_not_remove_line_breaks(binary:bin_to_list(FileContent), []).

read_file_lines_do_not_remove_line_breaks([], Acc) -> lists:reverse(Acc);
read_file_lines_do_not_remove_line_breaks(String, []) ->
    case string:split(String, "\n") of
        [[], RestLines] -> read_file_lines_do_not_remove_line_breaks(RestLines, [[]]);
        [Line, RestLines] -> read_file_lines_do_not_remove_line_breaks(RestLines, [[],Line]);
        [Line] -> Line
    end;
read_file_lines_do_not_remove_line_breaks(String, Acc) ->
    case string:split(String, "\n") of
        [[], RestLines] -> read_file_lines_do_not_remove_line_breaks(RestLines, [[]|Acc]);
        [Line, RestLines] -> read_file_lines_do_not_remove_line_breaks(RestLines, [[],Line|Acc]);
        [Line] -> [Line|Acc]
    end.

%% TODO Next refactoring step, function below should retrun lines with linebreaks
read_file_lines(FileName) ->
    {ok, FileContent} = read_file(FileName),
    string:tokens(erlang:binary_to_list(FileContent), "\n").

process_opts(Lines, []) -> Lines;
process_opts(Lines, [Opt|Opts]) ->
    case Opt of
        remove_line_breaks -> process_opts(lists:filter(fun(Line) -> Line =/= [] end, Lines), Opts);
        split_lines_into_words -> process_opts(lists:map(fun(Line) -> string:tokens(Line, " ") end, Lines), Opts);
        split_lines_into_characters -> Lines; %% TODO not_implementedprocess_opts(lists:map(fun(Line) -> string:tokens(Line, " ") end, Lines), Opts);
        _ -> exit({"Not supported option", Opt})
    end.

read_file_lines(FileName, Opts) ->
    InLines = read_file_lines_do_not_remove_line_breaks(FileName),
    process_opts(InLines, Opts).

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
    file_content_into_list_of_lines(FileContent, []).

file_content_into_list_of_lines([], Acc) -> lists:reverse(Acc);
file_content_into_list_of_lines(FileContent, Acc) ->
    case string:split(FileContent, "\n") of
        [Line, RestLines] -> file_content_into_list_of_lines(RestLines, [Line|Acc]);
        [Line] -> file_content_into_list_of_lines([], [Line|Acc])
    end.

%%-ifdef(TEST).
%%-include_lib("eunit/include/eunit.hrl").

file_content_into_list_of_lines_test() ->
    ?assertEqual([[], "ab", [], "cd", "ef"], file_content_into_list_of_lines("\nab\n\ncd\nef\n")),
    ?assertEqual(["ab", [], "cd", "ef", []], file_content_into_list_of_lines("ab\n\ncd\nef\n\n")).

remove_empty_lines_test() ->
    ?assertEqual(["ab", "cd", "ef"], remove_empty_lines([[], "ab", [], "cd", "ef"])),
    ?assertEqual(["ab", "cd", "ef"], remove_empty_lines(["ab", [], "cd", "ef", []])).

merge_functions_test() ->
    FileName = "test_input_day05.txt",
    Content1 = read_file_lines(FileName, [remove_line_breaks]),
    Content2 = read_file_lines(FileName),
    ?assertEqual(Content1, Content2).

read_file_lines_do_not_remove_line_breaks_test() ->
    FileName = "test_input_day01.txt",
    ?assertEqual(["1000", [],
                  "2000", [],
                  "3000", [],
                  [],
                  "4000", [],
                  [],
                  "5000", [],
                  "6000", [],
                  [],
                  "7000", [],
                  "8000", [],
                  "9000", [],
                  [],
                  "10000", []
                 ],
                 read_file_lines_do_not_remove_line_breaks(FileName)).

-endif.

