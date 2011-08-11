-module(data_broadcast).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    data_broadcast_sup:start_link(),
    % the firewall (iptables) is redirecting port 843 to 8843 for us
    % this is so we do not have to run erlang as root
    cowboy:start_listener(plcy, 128, 
			  cowboy_tcp_transport, [{port, 8843}],
			  socket_policy_server, []),
    cowboy:start_listener(svrv, 128, 
			  cowboy_tcp_transport, [{port, 8482}],
			  data_broadcast_incoming, []),
    cowboy:start_listener(clnt, 128, 
			  cowboy_tcp_transport, [{port, 5013}],
			  data_broadcast_outgoing, []).

stop(_State) ->
    ok.

start() ->
     application:start(cowboy),
     application:start(data_broadcast).
