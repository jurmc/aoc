-module(monkey).

-include_lib("monkey.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([start/1, terminate/1, init/1, take_turn/1, get_monkey/1, send_item/2]).

%%% Client API
start(Monkey = #monkey{}) ->
    Pid = spawn(?MODULE, init, [Monkey]),
    %%?debugFmt("Monkey started: Id ~p, Pid: ~p", [Monkey#monkey.id, Pid]),
    register(get_reg_name(Monkey#monkey.id), Pid),
    Pid.

terminate(Pid) ->
    Monkey = get_monkey(Pid),
    unregister(get_reg_name(Monkey#monkey.id)),
    erlang:exit(Pid, "terminate"),
    ok.

take_turn(Pid) ->
    From = {self(), make_ref()},
    Pid ! {From, take_turn},
    receive
        {From, take_turn_done} -> ok
    after 5000 ->
              erlang:error(timout)
    end.

get_monkey(Pid) ->
    From = {self(), make_ref()},
    Pid ! {From, get_monkey},
    receive
        {From, Monkey} -> Monkey
    after 5000 ->
              erlang:error(timout)
    end.

send_item(Pid, Item) ->
    From = {self(), make_ref()},
    Pid ! {From, item, Item},
    receive
        {From, ok} -> ok
    end.

%%% Server functions

init(Monkey = #monkey{}) ->
    loop(Monkey).

loop(Monkey) ->
    receive
        {{Pid, _Ref} = From, take_turn} ->
            NewMonkey = inspect_item(Monkey),
            Pid ! {From, take_turn_done},
            loop(NewMonkey);
        {{Pid, _Ref} = From, get_monkey} ->
            Pid ! {From, Monkey},
            loop(Monkey);
        {{Pid, _Ref} = From, item, Item} ->
            Pid ! {From, ok},
            NewItems = Monkey#monkey.items ++ [Item],
            loop(Monkey#monkey{items = NewItems})
    end.

%%% Private functions

get_reg_name(Id) ->
    list_to_atom(io_lib:format("monkey_~B", [Id])).

inspect_item(#monkey{items = []} = Monkey) -> Monkey;
inspect_item(#monkey{inspected = AlreadyInspected} = Monkey) ->
    [Item|RestItems] = Monkey#monkey.items,
    %%?debugFmt("Id: ~p -- Before WorryLevel1: Item: ~p Operand: ~p~n", [Monkey#monkey.id, Item, Monkey#monkey.operand]),
    Operand = case Monkey#monkey.operand of
                  "old" ->
                      %%?debugFmt("marker~n", []),
                      Item;
                  _ -> Monkey#monkey.operand
              end,
    WorryLevel1 = case Monkey#monkey.operator of
                      "+" -> Item + Operand;
                      "*" -> Item * Operand;
                      UnhandledOperand ->
                          %%?debugFmt("Unhandled operand: ~p~n", UnhandledOperand),
                          exit("Unhandled operand")
                  end,
    WorryLevel2 = erlang:trunc(WorryLevel1 / 3),
    %%?debugFmt("WorryLevel1: ~p WorryLevel2: ~p~n", [WorryLevel1, WorryLevel2]),
    case (WorryLevel2 rem Monkey#monkey.test) of
        0 -> send_to_monkey(Monkey#monkey.recipient_for_true, WorryLevel2);
        _ -> send_to_monkey(Monkey#monkey.recipient_for_false, WorryLevel2)
    end,
    inspect_item(Monkey#monkey{items=RestItems, inspected=AlreadyInspected+1}).

send_to_monkey(RecipientId, Item) ->
    %%?debugFmt("Sending Item: ~p, to monkey: ~p~n", [Item, RecipientId]),
    monkey:send_item(get_reg_name(RecipientId), Item).

%%% Unit tests

%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").

-define(TEST_MONKEY_1, #monkey{
                          id = 1,
                          items = [],
                          operator = "*",
                          operand = "old",
                          test = 13,
                          recipient_for_true = 1,
                          recipient_for_false = 3}).

-define(TEST_MONKEY_2, #monkey{
                          id = 2,
                          items = [79, 60, 97],
                          operator = "*",
                          operand = "old",
                          test = 13,
                          recipient_for_true = 1,
                          recipient_for_false = 3}).

-define(TEST_MONKEY_3, #monkey{
                          id = 3,
                          items = [200],
                          operator = "*",
                          operand = 19,
                          test = 13,
                          recipient_for_true = 2,
                          recipient_for_false = 3}).

get_monkey_test() ->
    Monkey = ?TEST_MONKEY_1,
    Pid = start(Monkey),
    ?assertEqual(Monkey, monkey:get_monkey(Pid)),
    terminate(Pid).

take_turn_test() ->
    Monkey1 = ?TEST_MONKEY_1,
    Monkey2 = ?TEST_MONKEY_2,
    Monkey3 = ?TEST_MONKEY_3,
    Pid1 = start(Monkey1),
    Pid2 = start(Monkey2),
    Pid3 = start(Monkey3),

    take_turn(Pid2),
    ExpectedMonkey1 = Monkey1#monkey{items=[2080]},
    ExpectedMonkey3 = Monkey3#monkey{items=[200, 1200, 3136]},
    ?assertEqual(3, (monkey:get_monkey(Pid2))#monkey.inspected),
    ?assertEqual(ExpectedMonkey1, monkey:get_monkey(Pid1)),
    ?assertEqual(ExpectedMonkey3, monkey:get_monkey(Pid3)),

    terminate(Pid1),
    terminate(Pid2),
    terminate(Pid3).

terminate_test() ->
    Monkey = ?TEST_MONKEY_1,
    Pid = start(Monkey),
    Registerd1 = registered(),
    ?assertEqual(true, lists:member(monkey_1, Registerd1)),

    terminate(Pid),
    Registerd2 = registered(),
    ?assertEqual(false, lists:member("monkey_1", Registerd2)).

-endif.
