%%%-------------------------------------------------------------------
%% @doc aoc_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(aoc_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc_erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
