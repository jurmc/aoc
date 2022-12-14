-module(aoc_input_app).

-export([read_file/1, read_file_lines/1, read_file_lines/2, read_file_lines2/1, read_file_lines_into_2_columns/1, remove_empty_lines/1]).

%%% Reading files

read_file(FileName) ->
    file:read_file(os:getenv("AOC_INPUT") ++ "/" ++ FileName).

read_file_lines(FileName) ->
    read_file_lines(FileName, []).

read_file_lines(FileName, Opts) ->
    InLines = read_file_lines_raw(FileName),
    process_opts(InLines, Opts).

%% TODO: Merge it with read_file_lines (above)
read_file_lines2(FileName) ->
    {ok, FileContent} = read_file(FileName),
    file_content_into_list_of_lines(FileContent).

%% TODO: Somethin more generic, so number of columns will be a passed in parameter?
read_file_lines_into_2_columns(FileName) ->
    Input = read_file_lines(FileName, [remove_line_breaks]),
    into_2_columns(Input).

%%% Conversions

%% TODO: Later this might be parametrized
%% TODO: or sucha converstions will become internal and read_file_lines/2 have to be used with proper Opts
into_2_columns(Lines) ->
    lists:map(fun(Line) -> [I1, I2] = string:tokens(Line, " "), {I1, I2} end, Lines).

remove_empty_lines(Lines) ->
    lists:filter(fun(X) -> [] =/= X end, Lines).

%%% Internal functions

%% TODO: probably to remove (we have read_file_lines with OPT now)
file_content_into_list_of_lines(FileContent) ->
    file_content_into_list_of_lines(FileContent, []).

%% TODO: probably to remove (we have read_file_lines with OPT now)
file_content_into_list_of_lines([], Acc) -> lists:reverse(Acc);
file_content_into_list_of_lines(FileContent, Acc) ->
    case string:split(FileContent, "\n") of
        [Line, RestLines] -> file_content_into_list_of_lines(RestLines, [Line|Acc]);
        [Line] -> file_content_into_list_of_lines([], [Line|Acc])
    end.

read_file_lines_raw(FileName) ->
    {ok, FileContent} = read_file(FileName),
    read_file_lines_raw(binary:bin_to_list(FileContent), []).

process_opts(Lines, []) -> Lines;
process_opts(Lines, [Opt|Opts]) ->
    case Opt of
        remove_line_breaks -> process_opts(lists:filter(fun(Line) -> Line =/= [] end, Lines), Opts);
        split_lines_into_words -> process_opts(lists:map(fun(Line) -> string:tokens(Line, " ") end, Lines), Opts);
        split_lines_into_characters -> Lines; %% TODO not_implementedprocess_opts(lists:map(fun(Line) -> string:tokens(Line, " ") end, Lines), Opts);
        _ -> exit({"Not supported option", Opt})
    end.

%% TODO: stupid name, use just read_file_lines (and yes, you still shouldn't remove line breaks)
%%       prposed new neame: read_file_lines_raq (?)
read_file_lines_raw([], Acc) -> lists:reverse(Acc);
read_file_lines_raw(String, []) ->
    case string:split(String, "\n") of
        [[], RestLines] -> read_file_lines_raw(RestLines, [[]]);
        [Line, RestLines] -> read_file_lines_raw(RestLines, [[],Line]);
        [Line] -> Line
    end;
read_file_lines_raw(String, Acc) ->
    case string:split(String, "\n") of
        [[], RestLines] -> read_file_lines_raw(RestLines, [[]|Acc]);
        [Line, RestLines] -> read_file_lines_raw(RestLines, [[],Line|Acc]);
        [Line] -> [Line|Acc]
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

file_content_into_list_of_lines_test() ->
    ?assertEqual([[], "ab", [], "cd", "ef"], file_content_into_list_of_lines("\nab\n\ncd\nef\n")),
    ?assertEqual(["ab", [], "cd", "ef", []], file_content_into_list_of_lines("ab\n\ncd\nef\n\n")).

remove_empty_lines_test() ->
    ?assertEqual(["ab", "cd", "ef"], remove_empty_lines([[], "ab", [], "cd", "ef"])),
    ?assertEqual(["ab", "cd", "ef"], remove_empty_lines(["ab", [], "cd", "ef", []])).

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
                 read_file_lines_raw(FileName)).

%%removal_read_file_lines2_test() -> %% TODO: Temporary test
%%    Out1 = read_file_lines2("test_input_day05.txt"),
%%    Out2 = read_file_lines("test_input_day05.txt"),
%%    ?assertEqual(Out1, Out2),
%%
%%    read_file_lines2("input_day05.txt").

-endif.

