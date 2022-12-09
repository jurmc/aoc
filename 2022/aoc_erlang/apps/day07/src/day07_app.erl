%%%-------------------------------------------------------------------
%% @doc day07 public API
%% @end
%%%-------------------------------------------------------------------

-module(day07_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    day07_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
