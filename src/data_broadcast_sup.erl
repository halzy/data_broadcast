
-module(data_broadcast_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Ports) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Ports]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([IDs]) ->
    DataPushers = [ {ID, {data_pusher, start_link, [ID]}, permanent, 5000, worker, [data_pusher]} || ID <- IDs ],
    {ok, { {one_for_one, 5, 10}, DataPushers} }.
