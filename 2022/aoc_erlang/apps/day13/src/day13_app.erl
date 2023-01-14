-module(day13_app).

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

read_input(FileName) ->
    InLines = aoc_input_app:read_file_lines(FileName, [remove_line_breaks]).

%% [ doDown
%% ] backUp
%%
%% d goForward
%% , closeAndGoForward

to_integer(Str) ->
    %%?debugFmt("Str: ~p", [Str]),
    {Int, []} = string:to_integer(Str),
    Int.

read_list(Line) ->
    {_RestLine, Items} = read_list(Line, {[], []}),
    ?debugFmt("Result Items: ~p", [Items]),
    Items.

%% {RestLine, Items} = read_list(Line, {Items = [], OngointItem = []}).
read_list([], {Items, []}) ->               % ]     - goStackUp
    ?debugFmt("Final: Items: ~p", [Items]),
    {[], Items};
read_list([$]|T], {Items, []}) ->               % ]     - goStackUp
    ?debugFmt("Mark1.1(])", []),
    {T, lists:reverse(Items)};
read_list([$]|T], {Items, OngoingDigits}) ->      % ]     - goStackUp
    ?debugFmt("Mark1.2(]): Items: ~p, OngoingDigits: ~p", [Items, OngoingDigits]),
    Int = to_integer(lists:reverse(OngoingDigits)),
    {T, lists:reverse([Int|Items])};
read_list([$[|T], {Items, OngoingDigits = ""}) -> % [     - goStackDown
    ?debugFmt("Mark2.1([): Items: ~p", [Items]),
    {NewT, NewItems} = read_list(T, {[], ""}),
    ?debugFmt("Mark2.2([):   NewT: ~p, NewItems: ~p", [NewT, NewItems]),
    read_list(NewT, {[NewItems|Items], ""});
read_list([$,|T], {Items, ""}) ->      % ,     - closeItem
    ?debugFmt("Mark3.1(,): Items: ~p", [Items]),
    read_list(T, {Items, ""});
read_list([$,|T], {Items, OngoingDigits}) ->      % ,     - closeItem
    ?debugFmt("Mark3.2(,): Items: ~p OngoingDigits: ~p", [Items, OngoingDigits]),
    Int = to_integer(lists:reverse(OngoingDigits)),
    read_list(T, {[Int|Items], []});
read_list([H|T], {Items, OngoingDigits}) ->       % digit - buildUpItem
    ?debugFmt("Mark4(digit)", []),
    read_list(T, {Items, [H|OngoingDigits]}).

read_list_test() ->
    Line = "[1,1,3,1,1]",
    ?assertEqual([1,1,3,1,1], read_list(Line)).

read_list_with_nested_list_test() ->
    Line = "[[1,2],[3,4,5]]",
    ?assertEqual([[1,2],[3,4,5]], read_list(Line)).

-endif.
