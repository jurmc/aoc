-module(day11_app).

-include_lib("monkey.hrl").

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

read_monkey([SectMonkeyId,SectItems,SectOperation,SectTest,SectRecipientTrue,SectRecipientFalse|T]) ->
    ["Monkey", Id] = SectMonkeyId,
    ["Starting", "items:"|Items] = SectItems,
    ["Operation:", "new", "=", "old", Operator, Operand] = SectOperation,
    ["Test:", "divisible", "by", Test] = SectTest,
    ["If", "true:", "throw", "to", "monkey", RecipientTrue] = SectRecipientTrue,
    ["If", "false:", "throw", "to", "monkey", RecipientFalse] = SectRecipientFalse,

    Monkey = #monkey{id                      = element(1, string:to_integer(Id)),
            items                   = [element(1, string:to_integer(Item)) || Item <- Items],
            operator               = Operator,
            operand                = element(1, string:to_integer(Operand)),
            test                    = element(1, string:to_integer(Test)),
            recipient_for_true      = element(1, string:to_integer(RecipientTrue)),
            recipient_for_false     = element(1, string:to_integer(RecipientFalse))},
    [Monkey, T]. 

read_monkeys(FileName) -> 
    Input = aoc_input_app: read_file_lines(FileName, [remove_line_breaks, split_lines_into_words]),
    read_monkeys(Input, []).

read_monkeys([], Acc) -> lists:reverse(Acc);
read_monkeys(Input, Acc) ->
    [Monkey, RestInput] = read_monkey(Input),
    read_monkeys(RestInput, [Monkey|Acc]).

monkey_read_test() ->
    [FirstMonkey|RestMonkeys] = read_monkeys("test_input_day11.txt"),
    ExpectedFirstMonkey = #monkey{
                        id = 0,
                        items = [79, 98],
                        operator = "*",
                        operand = 19,
                        test = 23,
                        recipient_for_true = 2,
                        recipient_for_false = 3},
    ?assertEqual(ExpectedFirstMonkey, FirstMonkey),

    LastMonkey = lists:last(RestMonkeys),
    ExpectedLastMonkey = #monkey{
                            id = 3,
                            items = [74],
                            operator = "+",
                            operand = 3,
                            test = 17,
                            recipient_for_true = 0,
                            recipient_for_false = 1},
    ?assertEqual(ExpectedLastMonkey, LastMonkey).

-endif.
