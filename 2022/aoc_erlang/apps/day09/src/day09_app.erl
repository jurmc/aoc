%%%-------------------------------------------------------------------
%% @doc day09 public API
%% @end
%%%-------------------------------------------------------------------

-module(day09_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    day09_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
