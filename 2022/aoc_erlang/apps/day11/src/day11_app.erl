-module(day11_app).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include_lib("monkey.hrl").

-export([part1/1, part2/1]).

%%% Exported functions

part1(FileName) ->
    Pids = start_all_monkeys(FileName, fun worry_level_postproc_for_part1/1),
    apply_n_rounds(20, Pids),
    [Max1,Max2|_] = lists:reverse(lists:sort([(monkey:get_monkey(Pid))#monkey.inspected|| Pid <- Pids])),
    [monkey:terminate(Pid) || Pid <- Pids],
    Max1*Max2.

part2(FileName) ->
    MonkeysRecordsWithoutPostProc = read_monkeys(FileName),
    Divisors = [Monkey#monkey.test || Monkey <- MonkeysRecordsWithoutPostProc],
    PostProcFun = fun(X) -> worry_level_postproc_for_part2(X, Divisors) end,
    MonkeysRecords = [Monkey#monkey{worry_level_postproc = PostProcFun} || Monkey <- MonkeysRecordsWithoutPostProc],
    Pids = [monkey:start(MonkeyRecord) || MonkeyRecord <- MonkeysRecords],

    apply_n_rounds(10000, Pids),
    [Max1,Max2|_] = lists:reverse(lists:sort([(monkey:get_monkey(Pid))#monkey.inspected|| Pid <- Pids])),
    [monkey:terminate(Pid) || Pid <- Pids],
    Max1*Max2.

%%% Internal functions

worry_level_postproc_for_part1(X) ->
        erlang:trunc(X / 3).

worry_level_postproc_for_part2(WorryLevel, Divisors) ->
    OperandsMultiplied = lists:foldl(fun(X, Acc) -> X * Acc end, 1, Divisors),
    case WorryLevel > OperandsMultiplied of
        true -> OperandsMultiplied + (WorryLevel rem OperandsMultiplied);
        _ -> WorryLevel
    end.


%%% Unit tests

%%-ifdef(TEST).
%%-include_lib("eunit/include/eunit.hrl").

read_monkey([SectMonkeyId,SectItems,SectOperation,SectTest,SectRecipientTrue,SectRecipientFalse|T]) ->
    ["Monkey", Id] = SectMonkeyId,
    ["Starting", "items:"|Items] = SectItems,
    ["Operation:", "new", "=", "old", Operator, Operand] = SectOperation,
    ["Test:", "divisible", "by", Test] = SectTest,
    ["If", "true:", "throw", "to", "monkey", RecipientTrue] = SectRecipientTrue,
    ["If", "false:", "throw", "to", "monkey", RecipientFalse] = SectRecipientFalse,

    Monkey = #monkey{id                      = element(1, string:to_integer(Id)),
            items                  = [element(1, string:to_integer(Item)) || Item <- Items],
            operator               = Operator,
            operand                = case Operand of
                                         "old" -> "old";
                                         _ -> element(1, string:to_integer(Operand))
                                     end,
            test                   = element(1, string:to_integer(Test)),
            recipient_for_true     = element(1, string:to_integer(RecipientTrue)),
            recipient_for_false    = element(1, string:to_integer(RecipientFalse))},
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

start_all_monkeys(FileName, Fun) ->
    MonkeysRecords = [Monkey#monkey{worry_level_postproc=Fun} || Monkey <- read_monkeys(FileName)],
    [monkey:start(MonkeyRecord) || MonkeyRecord <- MonkeysRecords].

start_all_monkeys_test() ->
    Pids = start_all_monkeys("test_input_day11.txt", fun worry_level_postproc_for_part1/1),
    ?assertEqual([true, true, true, true], [is_pid(Pid) || Pid <- Pids]),
    [monkey:terminate(Pid) || Pid <- Pids].

apply_n_rounds(0, Pids) -> Pids;
apply_n_rounds(N, Pids) ->
    [monkey:take_turn(Pid) || Pid <- Pids],
    apply_n_rounds(N-1, Pids).

first_round_test() ->
    [Pid0, Pid1, Pid2, Pid3] = Pids = start_all_monkeys("test_input_day11.txt", fun worry_level_postproc_for_part1/1),
    apply_n_rounds(1, Pids),

    Items0 = (monkey:get_monkey(Pid0))#monkey.items,
    Items1 = (monkey:get_monkey(Pid1))#monkey.items,
    Items2 = (monkey:get_monkey(Pid2))#monkey.items,
    Items3 = (monkey:get_monkey(Pid3))#monkey.items,

    ?assertEqual([20, 23, 27, 26], Items0),
    ?assertEqual([2080, 25, 167, 207, 401, 1046], Items1),
    ?assertEqual([], Items2),
    ?assertEqual([], Items3),

    [monkey:terminate(Pid) || Pid <- Pids].

twenty_rounds_test() ->
    [Pid0, Pid1, Pid2, Pid3] = Pids = start_all_monkeys("test_input_day11.txt", fun worry_level_postproc_for_part1/1),
    apply_n_rounds(20, Pids),

    Items0 = (monkey:get_monkey(Pid0))#monkey.items,
    Items1 = (monkey:get_monkey(Pid1))#monkey.items,
    Items2 = (monkey:get_monkey(Pid2))#monkey.items,
    Items3 = (monkey:get_monkey(Pid3))#monkey.items,

    ?assertEqual([10, 12, 14, 26, 34], Items0),
    ?assertEqual([245, 93, 53, 199, 115], Items1),
    ?assertEqual([], Items2),
    ?assertEqual([], Items3),

    InspectedList = [(monkey:get_monkey(Pid))#monkey.inspected|| Pid <- Pids],
    ?assertEqual([101, 95, 7, 105], InspectedList),

    [monkey:terminate(Pid) || Pid <- Pids].


part1_test() ->
    ?assertEqual(10605, part1("test_input_day11.txt")),
    ?assertEqual(69918, part1("input_day11.txt")).

part2_test() ->
    ?assertEqual(2713310158, part2("test_input_day11.txt")),
    ?assertEqual(19573408701, part2("input_day11.txt")).

worry_level_cut_test() ->
    Divisors = [23, 19, 13, 17],
    OperandsMultiplied = 96577,

    ?assertEqual(23, worry_level_postproc_for_part2(23, Divisors)),
    ?assertEqual(19, worry_level_postproc_for_part2(19, Divisors)),
    ?assertEqual(13, worry_level_postproc_for_part2(13, Divisors)),
    ?assertEqual(17, worry_level_postproc_for_part2(17, Divisors)),

    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied, Divisors)  rem 23),
    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied, Divisors)  rem 19),
    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied, Divisors)  rem 13),
    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied, Divisors)  rem 17),

    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied*10, Divisors)  rem 23),
    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied*200, Divisors)  rem 23),
    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied*30, Divisors) rem 19),
    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied*25, Divisors) rem 13),
    ?assertEqual(0, worry_level_postproc_for_part2(OperandsMultiplied*9, Divisors)  rem 17).

-endif.
