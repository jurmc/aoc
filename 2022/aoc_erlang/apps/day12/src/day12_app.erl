-module(day12_app).

-export([part1/1, part2/1]).

-define(UPPERCASE_S, 83).
-define(UPPERCASE_E, 69).
-define(LOWERCASE_A, 97).

%%% Exported functions

part1(_FileName) ->
    ok.

part2(_FileName) ->
    ok.

%%% Internal functions

get_ends(CoordsList) ->
    {value, {Begin, _}} = lists:search(fun({{X, Y}, Val}) -> Val =:= ?UPPERCASE_S end, CoordsList),
    {value, {End, _}} = lists:search(fun({{X, Y}, Val}) -> Val =:= ?UPPERCASE_E end, CoordsList),
    {Begin, End}.

normalize_height(CoordsList) ->
    lists:map(fun({{X, Y}, Val}) ->
                      case Val of
                          ?UPPERCASE_S -> {{X, Y}, 0};
                          ?UPPERCASE_E -> {{X, Y}, 25};
                          _ -> {{X, Y}, (Val - ?LOWERCASE_A)}
                      end
              end,
              CoordsList).

load_input(FileName) ->
    Lines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]),
    LinesWithY = [{Y, lists:nth(Y, Lines)} || Y <- lists:seq(1, length(Lines))],
    CoordsList = lists:foldl(fun({Y, Line}, Acc) ->
                                     Acc ++ [{{X, Y}, lists:nth(X, Line)} || X <- lists:seq(1, length(Line))]
                             end,
                             [],
                             LinesWithY),
    {Begin, End} = get_ends(CoordsList),
    HeightMatrix = normalize_height(CoordsList),
    {Begin, End, HeightMatrix}.

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

begin_end_coords_test() ->
    {Begin, End, _} = load_input("test_input_day12.txt"),
    ?assertEqual({1, 1}, Begin),
    ?assertEqual({6, 3}, End).

-endif.
