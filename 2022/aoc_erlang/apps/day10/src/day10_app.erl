-module(day10_app).

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

digest(Instructions) -> digest(Instructions, {1, []}).

digest([], {Curr, Acc}) -> lists:reverse([Curr|Acc]);
digest([H|T], {Curr, Acc}) ->
    ?debugFmt("H: ~p, T: ~p, Curr: ~p, Acc: ~p~n", [H, T, Curr, Acc]),
case H of
        "noop" -> digest(T, {Curr, [Curr|Acc]});
        {"addx", Val} ->  digest(T, {Curr+Val, [Curr,Curr|Acc]})
    end.

digest_test() ->
    Instructions = ["noop",
                    {"addx", 3},
                    {"addx", -5}],
    ?assertEqual([1, 1, 1, 4, 4, -1], digest(Instructions)).

load_input(FileName) ->
    Input = aoc_input_app:read_file_lines(FileName, [remove_line_breaks, split_lines_into_words]),
    lists:map(fun(Line) ->
                      case string:split(Line, " ") of
                          ["noop"] -> "noop";
                          ["addx", Val] -> {addx, element(1, string:to_integer(Val))}
                      end
              end,
              Input).

input_test() ->
    Input = load_input("test_input_day10.txt"),
    ?debugFmt("Input: ~p~n", [Input]),
    Digested = digest(Input),
    ?debugFmt("Digested: ~p~n", [Digested]),
    ok.


-endif.
