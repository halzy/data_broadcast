-module(data_broadcast).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    data_broadcast_sup:start_link().

stop(_State) ->
    ok.

start() ->
     application:start(data_broadcast).
