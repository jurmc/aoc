-module(day11_app).

-record(monkey, {id, items = [], operation, test, recipient_for_true, recipient_for_false}).

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

read_section([SectMonkeyId,SectItems,SectOperation,SectTest,SectRecipientTrue,SectRecipientFalse|T]) ->
    ?debugFmt("SectRecipientFalse: ~p~n", [SectRecipientFalse]),
    ["Monkey", Id] = SectMonkeyId,
    ["Starting", "items:"|Items] = SectItems,
    ["Operation:", "new", "=", "old", "*", Operation] = SectOperation,
    ["Test:", "divisible", "by", Test] = SectTest,
    ["If", "true:", "throw", "to", "monkey", RecipentTrue] = SectRecipientTrue,
    ["If", "false:", "throw", "to", "monkey", RecipentFales] = SectRecipientFalse,

    #monkey{id                      = element(1, string:to_integer(Id)),
            items                   = [element(1, string:to_integer(Item)) || Item <- SectItems],
            operation               = Operation,
            test                    = Test,
            recipient_for_true      = SectRecipientTrue,
            recipient_for_false     = SectRecipientFalse}.

read_monkey(FileName) ->
    Input = aoc_input_app: read_file_lines(FileName, [remove_line_breaks, split_lines_into_words]),
    read_section(Input).

monkey_read__test() ->
    ?assertEqual(#monkey{
                    id = 0,
                    items = [79, 78],
                    operation = 19,
                    test = 23,
                    recipient_for_true = 2,
                    recipient_for_false = 3},
                 read_monkey("test_input_day11.txt")).

-endif.
