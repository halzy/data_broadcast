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
    Result = data_broadcast_sup:start_link(),

    % the firewall (iptables) is redirecting port 843 to 8843 for us
    % this is so we do not have to run erlang as root
    PolicyPort = config(policy_port, 8443),
    ranch:start_listener(plcy, 128, 
              ranch_tcp, [{port, PolicyPort}],
              socket_policy_server, []),

    ConfigList = config(listeners, []),

    start_listener(ConfigList),

    Result.

list_config(List, Name, Default) ->
    case lists:keyfind(Name, 1, List) of
      false -> Default;
      {Name, Value} -> Value
    end.

start_listener([]) ->
  ok;
start_listener([{listen, Listen}|Listeners]) ->
  InPort = list_config(Listen, in, 5013),
  OutPort = list_config(Listen, out, 8482),
  WsPort = list_config(Listen, ws, 8002),

  Dispatch = [
      {'_', [
             %{['<<"raceview">>'], data_broadcast_websocket, [InPort]},
             {'_', data_broadcast_websocket, [InPort]}
             ]}
      ],
  ranch:start_listener("svrv_" ++ integer_to_list(InPort), 128,
            ranch_tcp, [{port, InPort}],
            data_broadcast_incoming, [InPort]),
  ranch:start_listener("clnt_" ++ integer_to_list(InPort), 128,
            ranch_tcp, [{port, OutPort}],
            data_broadcast_outgoing, [InPort]),
  cowboy:start_http("wbsk_" ++ integer_to_list(InPort), 128, 
            [{port, WsPort}], [{dispatch, Dispatch}]),
  start_listener(Listeners).

stop(_State) ->
    ok.

start() ->
  application:start(lager),
  application:start(ranch),
  application:start(cowboy),
  application:start(data_broadcast).
