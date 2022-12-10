-module(day07_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    {AllDirs, DirsDict, FilesDict} = process_entries(FileName),

    SizesList = sizes_list(AllDirs, DirsDict, FilesDict),
    SizesAtMost100000 = lists:filter(fun(Size) -> Size =< 100000 end, SizesList),
    lists:sum(SizesAtMost100000).

part2(FileName) ->
    {AllDirs, DirsDict, FilesDict} = process_entries(FileName),

    TotalSpace = 70000000,
    CurrOccupiedSpace = get_size_for_dir(["/"], DirsDict, FilesDict),
    CurrFreeSpace = TotalSpace - CurrOccupiedSpace,

    MinReqSpace = 30000000,
    CurrReqSpace = MinReqSpace - CurrFreeSpace,

    SizesList = sizes_list(AllDirs, DirsDict, FilesDict),
    lists:min(lists:sort(lists:filter(fun(Size) -> Size >= CurrReqSpace end, SizesList))).

%%% Internal functions

sizes_list(AllDirs, DirsDict, FilesDict) ->
    lists:map(fun(Dir) -> get_size_for_dir(Dir, DirsDict, FilesDict) end, AllDirs).

process_entries(FileName) ->
    Lines = aoc_input_app:read_file_lines(FileName, [split_lines_into_words]),
    ForDirs = process_entries_for_dirs(Lines),
    ForFiles = process_entries_for_files(Lines),
    Dirs = orddict:fetch_keys(ForDirs),
    {Dirs, ForDirs, ForFiles}.

process_entries_for_files(ContexEntries) ->
    {_, Result} = lists:foldl(fun(["$", "cd", "/"], {[], Acc}) ->
                                      {["/"], orddict:store(["/"], [], Acc)};
                                 (["$", "ls"], {Cwd, Acc}) ->
                                      {Cwd, Acc};
                                 (["$", "cd", ".."], {Cwd, Acc}) ->
                                      UpdatedCwd = lists:reverse(tl(lists:reverse(Cwd))),
                                      {UpdatedCwd, Acc};
                                 (["$", "cd", NewDir], {Cwd, Acc}) ->
                                      UpdatedCwd = lists:append(Cwd, [NewDir]),
                                      {UpdatedCwd, orddict:store(UpdatedCwd, [], Acc)};
                                 (["dir", _], {Cwd, Acc}) ->
                                      {Cwd, Acc};
                                 ([Size, File], {Cwd, Acc}) ->
                                      {CurrDirFiles, Dict} = orddict:take(Cwd, Acc),
                                      UpdatedDirFiles = lists:append(CurrDirFiles, [{Size, File}]),
                                      {Cwd, orddict:store(Cwd, UpdatedDirFiles, Dict)}
                              end,
                              {[], orddict:from_list([])},
                              ContexEntries),
    Result.

process_entries_for_dirs(ContexEntries) ->
    {_, Result} = lists:foldl(fun(["$", "cd", "/"], {[], Acc}) ->
                                      {["/"], orddict:store(["/"], [], Acc)};
                                 (["$", "ls"], {Cwd, Acc}) ->
                                      {Cwd, Acc};
                                 (["$", "cd", ".."], {Cwd, Acc}) ->
                                      UpdatedCwd = lists:reverse(tl(lists:reverse(Cwd))),
                                      {UpdatedCwd, Acc};
                                 (["$", "cd", NewDir], {Cwd, Acc}) ->
                                      UpdatedCwd = lists:append(Cwd, [NewDir]),
                                      {UpdatedCwd, orddict:store(UpdatedCwd, [], Acc)};
                                 (["dir", Dir], {Cwd, Acc}) ->
                                      {CurrDirDirs, Dict} = orddict:take(Cwd, Acc),
                                      UpdatedDirDirs = lists:append(CurrDirDirs, [Dir]),
                                      {Cwd, orddict:store(Cwd, UpdatedDirDirs, Dict)};
                                 ([_, _], {Cwd, Acc}) ->
                                      {Cwd, Acc}
                              end,
                              {[], orddict:from_list([])},
                              ContexEntries),
    Result.

get_size_for_dir(Dir, DirsDict, FilesDict) ->
    {CurDirFiles, _} = orddict:take(Dir, FilesDict),
    SumSizeForFiles = lists:foldl(fun(Item, Acc) ->
                                          {SizeStr, _} = Item,
                                          {Size, _} = string:to_integer(SizeStr),
                                          Acc + Size
                                  end,
                                  0,
                                  CurDirFiles),

    {CurDirDirs, _} = orddict:take(Dir, DirsDict),
    SumSizeForDirs = lists:foldl(fun(Item, Acc) ->
                                         DirToCheck = lists:append(Dir, [Item]),
                                         Acc + get_size_for_dir(DirToCheck, DirsDict, FilesDict)
                                 end,
                                 0,
                                 CurDirDirs),

    SumSizeForDirs + SumSizeForFiles.


%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_entries_for_files_test() ->
    Lines = aoc_input_app:read_file_lines("test_input_day07.txt", [split_lines_into_words]),
    ExpectedFiles = orddict:from_list(
                     [
                                {["/"],                                    [{"14848514", "b.txt"}, {"8504156", "c.dat"}]},
                      {     ["/", "a"],                              [{"29116", "f"}, {"2557", "g"}, {"62596", "h.lst"}]},
                      {["/", "a", "e"],                                                                   [{"584", "i"}]},
                      {     ["/", "d"], [{"4060174", "j"}, {"8033020", "d.log"}, {"5626152", "d.ext"}, {"7214296", "k"}]}
                     ]),
    Dirs = process_entries_for_files(Lines),
    ?assertEqual(ExpectedFiles, Dirs).

process_entries_for_dirs_test() ->
    Lines = aoc_input_app:read_file_lines("test_input_day07.txt", [split_lines_into_words]),
    ExpectedDirs = orddict:from_list(
                     [
                      {          ["/"], ["a", "d"]},
                      {     ["/", "a"],      ["e"]},
                      {["/", "a", "e"],         []},
                      {     ["/", "d"],         []}
                     ]),
    Dirs = process_entries_for_dirs(Lines),
    ?assertEqual(ExpectedDirs, Dirs).

get_size_for_dir_test() ->
    {_, DirsDict, FilesDict} = process_entries("test_input_day07.txt"),

    Dir1 = ["/", "a", "e"],
    ?assertEqual(584, get_size_for_dir(Dir1, DirsDict, FilesDict)),


    Dir2 = ["/"],
    ?assertEqual(48381165, get_size_for_dir(Dir2, DirsDict, FilesDict)).

part1_test() ->
    ?assertEqual(95437, part1("test_input_day07.txt")),
    ?assertEqual(1490523, part1("input_day07.txt")).

part2_test() ->
    ?assertEqual(24933642, part2("test_input_day07.txt")),
    ?assertEqual(12390492, part2("input_day07.txt")).

-endif.
