%%%-------------------------------------------------------------------
%% @doc supervisors public API
%% @end
%%%-------------------------------------------------------------------

-module(supervisors_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    supervisors_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
