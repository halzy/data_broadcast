-module(data_broadcast).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0]).

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

start() -> start(?MODULE).
start(App) ->
    start_ok(App, application:start(App, permanent)).
start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) -> 
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) -> 
    erlang:error({app_start_failed, App, Reason}).
stop() -> 
    application:stop(?MODULE).
