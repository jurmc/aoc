-module(day10_app).

-export([part1/1, part2/1]).

-define(CRT_WIDTH, 40).
-define(PIXEL_ON, 35).
-define(PIXEL_OFF, 46).

%%% Exported functions

%%% Internal functions

%%% Unit tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

x_reg_values(Instructions) -> x_reg_values(Instructions, {1, []}).

x_reg_values([], {Curr, Acc}) -> lists:reverse([Curr|Acc]);
x_reg_values([H|T], {Curr, Acc}) ->
    Vals = case H of
               noop -> x_reg_values(T, {Curr, [Curr|Acc]});
               {addx, Val} ->  x_reg_values(T, {Curr+Val, [Curr,Curr|Acc]})
           end,
    AllowedXMin = -1,
    AllowedXMax = 39,

    case AllowedXMin > lists:min(Vals) of
        true -> error("Wrong value of X register: too much");
        _ -> ok
    end,

    case AllowedXMax < lists:max(Vals) of
        true -> error("Wrong value of X register: too little");
        _ -> ok
    end,

    Vals.

digest_test() ->
    Instructions = [noop,
                    {addx, 3},
                    {addx, -5}],
    ?assertEqual([1, 1, 1, 4, 4, -1], x_reg_values(Instructions)).

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
    XRegVals = x_reg_values(Input),
    ?assertEqual(21, lists:nth(20, XRegVals)),
    ?assertEqual(19, lists:nth(60, XRegVals)),
    ?assertEqual(18, lists:nth(100, XRegVals)),
    ?assertEqual(21, lists:nth(140, XRegVals)),
    ?assertEqual(16, lists:nth(180, XRegVals)),
    ?assertEqual(18, lists:nth(220, XRegVals)).

part1(FileName) ->
    XRegVals = x_reg_values(InputInput = load_input(FileName)),
    Cycles = [20, 60, 100, 140, 180, 220],
    lists:sum([Cycle * lists:nth(Cycle, XRegVals) ||  Cycle <- Cycles]).

part1_test() ->
    ?assertEqual(13140, part1("test_input_day10.txt")),
    ?assertEqual(13720, part1("input_day10.txt")).

sprite_position_for_cycle(Cycle, XRegVals) ->
    case lists:nth(Cycle, XRegVals) of
        N -> sets:from_list([N-1, N, N+1])
    end.

sprite_position_for_cycle_test() ->
    XRegVals = x_reg_values(load_input("test_input_day10.txt")),
    ?assertEqual(sets:from_list([0,1,2]), sprite_position_for_cycle(1, XRegVals)),
    ?assertEqual(sets:from_list([0,1,2]), sprite_position_for_cycle(2, XRegVals)),
    ?assertEqual(sets:from_list([15,16,17]), sprite_position_for_cycle(3, XRegVals)).

render_image(XRegVals) ->
    PixelsInOneRow = lists:map(fun(Cycle) ->
                                       CRTCol = (Cycle - 1) rem ?CRT_WIDTH,
                                       CRTRow = (Cycle - 1) div ?CRT_WIDTH,
                                       SpritePos = sprite_position_for_cycle(Cycle, XRegVals),
                                       Pixel = case sets:is_element(CRTCol, SpritePos) of
                                                   true -> ?PIXEL_ON;
                                                   false  -> ?PIXEL_OFF
                                               end
                               end,
                               lists:seq(1,240)),
    NumRows = length(PixelsInOneRow) div ?CRT_WIDTH,
    Pixels = [lists:sublist(PixelsInOneRow, 1 + (RowNum * ?CRT_WIDTH), ?CRT_WIDTH) || RowNum <- lists:seq(0, NumRows-1)],
    Pixels.

part2(FileName) ->
    render_image(x_reg_values(load_input(FileName))).


part2_test() ->
    ExpectedTestImage = ["##..##..##..##..##..##..##..##..##..##..",
                         "###...###...###...###...###...###...###.",
                         "####....####....####....####....####....",
                         "#####.....#####.....#####.....#####.....",
                         "######......######......######......####",
                         "#######.......#######.......#######....."],
    ?assertEqual(ExpectedTestImage, part2("test_input_day10.txt")),

    ExpectedImage = ["####.###..#..#.###..#..#.####..##..#..#.",
                     "#....#..#.#..#.#..#.#..#....#.#..#.#..#.",
                     "###..###..#..#.#..#.####...#..#....####.",
                     "#....#..#.#..#.###..#..#..#...#....#..#.",
                     "#....#..#.#..#.#.#..#..#.#....#..#.#..#.",
                     "#....###...##..#..#.#..#.####..##..#..#."],
    ?assertEqual(ExpectedImage, part2("input_day10.txt")).






-endif.
