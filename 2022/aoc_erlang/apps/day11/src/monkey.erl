-module(monkey).

-include_lib("monkey.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([start/1, init/1, take_turn/1, get_monkey/1, send_item/2]).

%%% Client API
start(Monkey = #monkey{}) ->
    Pid = spawn(?MODULE, init, [Monkey]),
    register(get_reg_name(Monkey#monkey.id), Pid),
    Pid.

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
inspect_item(Monkey) ->
    [Item|RestItems] = Monkey#monkey.items,
    WorryLevel1 = case Monkey#monkey.operator of
                      "+" -> Item + Monkey#monkey.operand;
                      "*" -> Item * Monkey#monkey.operand
                  end,
    WorryLevel2 = erlang:trunc(WorryLevel1 / 3),
    case WorryLevel2 rem 23 of
        1 -> send_to_monkey(Monkey#monkey.recipient_for_true, WorryLevel2);
        _ -> send_to_monkey(Monkey#monkey.recipient_for_false, WorryLevel2)
    end,
    inspect_item(Monkey#monkey{items=RestItems}).

send_to_monkey(RecipientId, Item) ->
    monkey:send_item(get_reg_name(RecipientId), Item).

%%% Unit tests

%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").

-define(TEST_MONKEY_0, #monkey{
                          id = 0,
                          items = [79, 98],
                          operator = "*",
                          operand = 19,
                          test = 23,
                          recipient_for_true = 2,
                          recipient_for_false = 3}).

-define(TEST_MONKEY_2, #monkey{
                          id = 2,
                          items = [],
                          operator = "*",
                          operand = 19,
                          test = 23,
                          recipient_for_true = 2,
                          recipient_for_false = 3}).

-define(TEST_MONKEY_3, #monkey{
                          id = 3,
                          items = [200],
                          operator = "*",
                          operand = 19,
                          test = 23,
                          recipient_for_true = 2,
                          recipient_for_false = 3}).

get_monkey_test() ->
    Monkey = ?TEST_MONKEY_0,
    Pid = start(Monkey),
    ?assertEqual(Monkey, monkey:get_monkey(Pid)),
    exit(Pid, exit).

take_turn_test() ->
    Monkey0 = ?TEST_MONKEY_0,
    Monkey2 = ?TEST_MONKEY_2,
    Monkey3 = ?TEST_MONKEY_3,
    Pid0 = start(Monkey0),
    Pid2 = start(Monkey2),
    Pid3 = start(Monkey3),

    take_turn(Pid0),
    ExpectedMonkey2 = Monkey2,
    ExpectedMonkey3 = Monkey3#monkey{items=[200, 500, 620]},
    ?assertEqual(ExpectedMonkey2, monkey:get_monkey(Pid2)),
    ?assertEqual(ExpectedMonkey3, monkey:get_monkey(Pid3)),
    exit(Pid0, exit),
    exit(Pid2, exit),
    exit(Pid3, exit).

-endif.
