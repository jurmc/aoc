-module(day07_app).

%%-export([part1/1, part2/1]).

%%% Exported functions

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% $ ls
%% dir a
%% 14848514 b.txt
%% 8504156 c.dat
%% dir d
%% $ cd a

process_dir_listing(DirListing) ->
    [Dirs, Files] = lists:foldl(fun(Item, [Dirs, Files]) ->
                        case Item of
                            ["dir", DirName] -> [[DirName|Dirs], Files];
                            [FileSize, FileName] -> [Dirs, [{FileName, element(1, string:to_integer(FileSize))}|Files]];
                            NotImplemented -> ?debugFmt("NotImplemented: ~p\n", [NotImplemented])
                        end
                end,
                [[], []],
                DirListing),
    [lists:reverse(Dirs), lists:reverse(Files)].

process_dir_listing_test() ->
    DirListing = [["dir", "a"], ["14848514", "b.txt"], ["8504156", "c.dat"], ["dir", "d"]],
    ExpectedDirContent = [["a", "d"], [
                             {"b.txt", 14848514},
                             {"c.dat", 8504156}
                                      ]
                         ],

    ?assertEqual(ExpectedDirContent, process_dir_listing(DirListing)).

process_contex(ContexEntries) ->
    ContextLines = lists:filter(fun(Line) ->
                                        case Line of
                                            ["$", "cd", _] -> true;
                                            _ -> false
                                        end
                                end,
                                ContexEntries),
    {_, ReversedContexts} = lists:foldl(fun(Context, {_, []}) ->
                                        [_, _, Dir] = Context,
                                        {[Dir], [Dir]};
                                   (Context, {Cwd, Contexts}) ->
                                        [_, _, Dir] = Context,
                                        case Dir of
                                            ".." -> {tl(Cwd), Contexts};
                                            _ -> {[Dir|Cwd], [[Dir|Cwd]|Contexts]}
                                        end
                                end,
                                {"", []},
                                ContextLines),
    lists:reverse(lists:map(fun(Line) -> lists:reverse(Line) end, ReversedContexts)).

process_contex_test() ->
    Lines = aoc_input_app:read_file_lines("test_input_day07.txt", [split_lines_into_words]),
    Contexts = process_contex(Lines),

    ExpectedContexts = ["/", ["/", "a"], ["/", "a", "e"], ["/", "d"]],
    ?assertEqual(ExpectedContexts, Contexts).

process_entries_for_files(ContexEntries) ->
    lists:foldl(fun(["$", "cd", "/"], {[], Acc}) ->
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
                ContexEntries).

process_entries_for_files_test() ->
    Lines = aoc_input_app:read_file_lines("test_input_day07.txt", [split_lines_into_words]),
    ExpectedFiles = orddict:from_list(
                     [
                                {["/"],                                    [{"14848514", "b.txt"}, {"8504156", "c.dat"}]},
                      {     ["/", "a"],                              [{"29116", "f"}, {"2557", "g"}, {"62596", "h.lst"}]},
                      {["/", "a", "e"],                                                                   [{"584", "i"}]},
                      {     ["/", "d"], [{"4060174", "j"}, {"8033020", "d.log"}, {"5626152", "d.ext"}, {"7214296", "k"}]}
                     ]),
    {_, Dirs} = process_entries_for_files(Lines),
    ?assertEqual(ExpectedFiles, Dirs).

process_entries_for_dirs(ContexEntries) ->
    lists:foldl(fun(["$", "cd", "/"], {[], Acc}) ->
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
                ContexEntries).

process_entries_for_dirs_test() ->
    Lines = aoc_input_app:read_file_lines("test_input_day07.txt", [split_lines_into_words]),
    ExpectedDirs = orddict:from_list(
                     [
                      {          ["/"], ["a", "d"]},
                      {     ["/", "a"],      ["e"]},
                      {["/", "a", "e"],         []},
                      {     ["/", "d"],         []}
                     ]),
    {_, Dirs} = process_entries_for_dirs(Lines),
    ?assertEqual(ExpectedDirs, Dirs).

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

get_size_for_dir_test() ->
    Lines = aoc_input_app:read_file_lines("test_input_day07.txt", [split_lines_into_words]),
    {_, DirsDict} = process_entries_for_dirs(Lines),
    {_, FilesDict} = process_entries_for_files(Lines),

    Dir1 = ["/", "d"],
    ?assertEqual(4060174 + 8033020 + 5626152 + 7214296, get_size_for_dir(Dir1, DirsDict, FilesDict)),

    Dir2 = ["/", "a", "e"],
    ?assertEqual(584, get_size_for_dir(Dir2, DirsDict, FilesDict)),

    Dir3 = ["/", "a"],
    ?assertEqual(94853, get_size_for_dir(Dir3, DirsDict, FilesDict)),

    Dir4 = ["/", "d"],
    ?assertEqual(24933642, get_size_for_dir(Dir4, DirsDict, FilesDict)),

    Dir5 = ["/"],
    ?assertEqual(48381165, get_size_for_dir(Dir5, DirsDict, FilesDict)).

part1(FileName) ->
    Lines = aoc_input_app:read_file_lines(FileName, [split_lines_into_words]),
    {_, DirsDict} = process_entries_for_dirs(Lines),
    {_, FilesDict} = process_entries_for_files(Lines),

    AllDirs = orddict:fetch_keys(DirsDict),
    SizesList =lists:map(fun(Dir) ->
                                 get_size_for_dir(Dir, DirsDict, FilesDict)
                         end,
                         AllDirs),
    SizesAtMost100000 = lists:filter(fun(Size) -> Size =< 100000 end, SizesList),
    lists:sum(SizesAtMost100000).

part1_test() ->
    TestResult = part1("test_input_day07.txt"),
    ?debugFmt("Part1 test result: ~p\n", [TestResult]),
    ?assertEqual(95437, TestResult),

    Result = part1("input_day07.txt"),
    ?debugFmt("Part1 result: ~p\n", [Result]),
    ?assertEqual(1490523, Result).

part2(FileName) ->
    Lines = aoc_input_app:read_file_lines(FileName, [split_lines_into_words]),
    {_, DirsDict} = process_entries_for_dirs(Lines),
    {_, FilesDict} = process_entries_for_files(Lines),

    TotalSpace = 70000000,
    CurrOccupiedSpace = get_size_for_dir(["/"], DirsDict, FilesDict),
    CurrFreeSpace = TotalSpace - CurrOccupiedSpace,

    MinReqSpace = 30000000,
    CurrReqSpace = MinReqSpace - CurrFreeSpace,

    AllDirs = orddict:fetch_keys(DirsDict),
    SizesList = lists:map(fun(Dir) ->
                                  get_size_for_dir(Dir, DirsDict, FilesDict)
                          end,
                          AllDirs),
    SizeEnoughList = lists:min(lists:sort(lists:filter(fun(Size) -> Size >= CurrReqSpace end, SizesList))).

part2_test() ->
    TestResult = part2("test_input_day07.txt"),
    ?debugFmt("Part2 test result: ~p\n", [TestResult]),
    ?assertEqual(24933642, TestResult),

    Result = part2("input_day07.txt"),
    ?debugFmt("Part2 result: ~p\n", [Result]),
    ?assertEqual(12390492, Result).

-endif.
