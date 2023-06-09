%%%-------------------------------------------------------------------
%% @doc friend_storage public API
%% @end
%%%-------------------------------------------------------------------

-module(friend_storage_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    friend_storage_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
