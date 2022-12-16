-module(day10_app).

-export([part1/1, part2/1]).

%%% Exported functions

part2(_FileName) ->
    ok.

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

digest(Instructions) -> digest(Instructions, {1, []}).

digest([], {Curr, Acc}) -> lists:reverse([Curr|Acc]);
digest([H|T], {Curr, Acc}) ->
    case H of
        noop -> digest(T, {Curr, [Curr|Acc]});
        {addx, Val} ->  digest(T, {Curr+Val, [Curr,Curr|Acc]})
    end.

digest_test() ->
    Instructions = [noop,
                    {addx, 3},
                    {addx, -5}],
    ?assertEqual([1, 1, 1, 4, 4, -1], digest(Instructions)).

load_input(FileName) ->
    Input = aoc_input_app:read_file_lines(FileName, [remove_line_breaks, split_lines_into_words]),
    lists:map(fun(Line) ->
                      case string:split(Line, " ") of
                          [["noop"]] -> noop;
                          [["addx", Val]] -> {addx, element(1, string:to_integer(Val))};
                          What -> ?debugFmt("Got it: ~p~n", [What])
                      end
              end,
              Input).

register_value_at_cycle_test() ->
    Input = load_input("test_input_day10.txt"),
    Digested = digest(Input),
    %%?debugFmt("Digested: ~p~n", [Digested]),
    ?assertEqual(21, lists:nth(20, Digested)),
    ?assertEqual(19, lists:nth(60, Digested)),
    ?assertEqual(18, lists:nth(100, Digested)),
    ?assertEqual(21, lists:nth(140, Digested)),
    ?assertEqual(16, lists:nth(180, Digested)),
    ?assertEqual(18, lists:nth(220, Digested)).

part1(FileName) ->
    Digested = digest(InputInput = load_input(FileName)),
    Cycles = [20, 60, 100, 140, 180, 220],
    lists:sum([Cycle * lists:nth(Cycle, Digested) ||  Cycle <- Cycles]).

part1_test() ->
    ?assertEqual(13140, part1("test_input_day10.txt")),
    ?assertEqual(13720, part1("input_day10.txt")).

-endif.
