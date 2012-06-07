-module(data_broadcast).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

config(Name, Default) ->
    case application:get_env(?MODULE, Name) of
	{ok, Value} -> Value;
	undefined -> Default
    end.

start(_StartType, _StartArgs) ->

    % the firewall (iptables) is redirecting port 843 to 8843 for us
    % this is so we do not have to run erlang as root
    PolicyPort = config(policy_port, 8443),
    InPort = config(in_port, 5013),
    OutPort = config(out_port, 8482),
    WsPort = config(ws_port, 8002),

    data_broadcast_sup:start_link(),
    Dispatch = [
		{'_', [
		       {[<<"raceview">>], data_broadcast_websocket, []},
		       {'_', default_handler, []}
		       ]}
		],
    cowboy:start_listener(plcy, 128, 
			  cowboy_tcp_transport, [{port, PolicyPort}],
			  socket_policy_server, []),
    cowboy:start_listener(svrv, 128, 
			  cowboy_tcp_transport, [{port, InPort}],
			  data_broadcast_incoming, []),
    cowboy:start_listener(wbks, 128, 
			  cowboy_tcp_transport, [{port, WsPort}],
			  cowboy_http_protocol, [{dispatch, Dispatch}]),
    cowboy:start_listener(clnt, 128, 
			  cowboy_tcp_transport, [{port, OutPort}],
			  data_broadcast_outgoing, []).

stop(_State) ->
    ok.

start() ->
     application:start(cowboy),
     application:start(data_broadcast).
