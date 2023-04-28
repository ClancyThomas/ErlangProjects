%%%-------------------------------------------------------------------
%% @doc cone_combo public API
%% @end
%%%-------------------------------------------------------------------

-module(cone_combo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cone_combo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
