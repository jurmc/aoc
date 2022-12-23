-module(monkey).
-behaviour(gen_server).

-include_lib("monkey.hrl").

-export([start/1, terminate/1, take_turn/1, get_monkey/1, send_item/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%%% Client API
start(Monkey = #monkey{}) ->
    ServName = {local, get_reg_name(Monkey#monkey.id)},
    {ok, Pid} = gen_server:start(ServName, ?MODULE, Monkey, []),
    Pid.

terminate(Pid) ->
    gen_server:call(Pid, terminate).

take_turn(Pid) ->
    gen_server:call(Pid, take_turn).

get_monkey(Pid) ->
    gen_server:call(Pid, get_monkey).

send_item(Pid, Item) ->
    gen_server:call(Pid, {item, Item}).

%%% Server functions

init(Monkey = #monkey{}) ->
    {ok, Monkey}.

handle_call(take_turn, _From, Monkey) ->
    {reply, ok, inspect_item(Monkey)};
handle_call(get_monkey, _From, Monkey) ->
    {reply, Monkey, Monkey};
handle_call({item, Item}, _From, Monkey = #monkey{items = Items}) ->
    {reply, ok, Monkey#monkey{items = Items ++ [Item]}};
handle_call(terminate, _From, Monkey) ->
    {stop, normal, ok, Monkey}.

handle_cast(Msg, Monkey) ->
    io:format("Unexpected message (~p) received (in state ~p)~n", [Msg, Monkey]),
    {noreply, Monkey}.

terminate(normal, _Monkey) ->
    ok.

%%% Private functions

get_reg_name(Id) ->
    list_to_atom(io_lib:format("monkey_~B", [Id])).

inspect_item(#monkey{items = []} = Monkey) -> Monkey;
inspect_item(#monkey{inspected = AlreadyInspected} = Monkey) ->
    [Item|RestItems] = Monkey#monkey.items,
    Operand = case Monkey#monkey.operand of
                  "old" ->
                      Item;
                  _ -> Monkey#monkey.operand
              end,
    WorryLevel1 = case Monkey#monkey.operator of
                      "+" -> Item + Operand;
                      "*" -> Item * Operand;
                      _UnhandledOperand ->
                          exit("Unhandled operand")
                  end,
    WorryLevel2 = case Monkey#monkey.worry_level_postproc of
                      undefined -> erlang:trunc(WorryLevel1 / 3); %% TODO: get rid of this undefined clause
                      _ -> (Monkey#monkey.worry_level_postproc)(WorryLevel1)
                  end,
    case (WorryLevel2 rem Monkey#monkey.test) of
        0 -> send_to_monkey(Monkey#monkey.recipient_for_true, WorryLevel2);
        _ -> send_to_monkey(Monkey#monkey.recipient_for_false, WorryLevel2)
    end,
    inspect_item(Monkey#monkey{items=RestItems, inspected=AlreadyInspected+1}).

send_to_monkey(RecipientId, Item) ->
    monkey:send_item(get_reg_name(RecipientId), Item).

%%% Unit tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
