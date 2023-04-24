%%%-------------------------------------------------------------------
%% @doc arithmetic public API
%% @end
%%%-------------------------------------------------------------------

-module(arithmetic_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    arithmetic_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
