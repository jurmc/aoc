-module(day07_app).

-export([part1/1, part2/1]).

%%% Exported functions

part1(_FileName) ->
    ok.

part2(_FileName) ->
    ok.

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

-endif.
