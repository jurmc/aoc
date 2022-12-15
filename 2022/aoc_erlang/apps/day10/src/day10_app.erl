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

first_test() ->
    ?assertEqual(ok, ok).

-endif.
