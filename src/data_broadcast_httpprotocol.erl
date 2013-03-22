-module(data_broadcast_httpprotocol).

-export([start_link/4]).
-export([init/4]). 

-record(state, {
    socket :: inet:socket(),
    transport :: module(),
    middlewares :: [module()],
    compress :: boolean(),
    env :: cowboy_middleware:env(),
    onrequest :: undefined | cowboy_protocol:onrequest_fun(),
    onresponse = undefined :: undefined | cowboy_protocol:onresponse_fun(),
    max_empty_lines :: non_neg_integer(),
    req_keepalive = 1 :: non_neg_integer(),
    max_keepalive :: non_neg_integer(),
    max_request_line_length :: non_neg_integer(),
    max_header_name_length :: non_neg_integer(),
    max_header_value_length :: non_neg_integer(),
    max_headers :: non_neg_integer(),
    timeout :: timeout(),
    until :: non_neg_integer() | infinity
}).

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%% @doc Taken from cowboy_protocol
%% @private
get_value(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        _ -> Default
    end.
%% @doc Taken from cowboy_protocol
%% @private
-spec until(timeout()) -> non_neg_integer() | infinity.
until(infinity) ->
    infinity;
until(Timeout) ->
    {Me, S, Mi} = os:timestamp(),
    Me * 1000000000 + S * 1000 + Mi div 1000 + Timeout.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Opts) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Taken from cowboy_protocol <BEGIN>
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    Compress = get_value(compress, Opts, false),
    MaxEmptyLines = get_value(max_empty_lines, Opts, 5),
    MaxHeaderNameLength = get_value(max_header_name_length, Opts, 64),
    MaxHeaderValueLength = get_value(max_header_value_length, Opts, 4096),
    MaxHeaders = get_value(max_headers, Opts, 100),
    MaxKeepalive = get_value(max_keepalive, Opts, 100),
    MaxRequestLineLength = get_value(max_request_line_length, Opts, 4096),
    Middlewares = get_value(middlewares, Opts, [cowboy_router, cowboy_handler]),
    Env = [{listener, ListenerPid}|get_value(env, Opts, [])],
    OnRequest = get_value(onrequest, Opts, undefined),
    OnResponse = get_value(onresponse, Opts, undefined),
    Timeout = get_value(timeout, Opts, 5000),
    ok = ranch:accept_ack(ListenerPid),

    State = #state{socket=Socket, transport=Transport,
        middlewares=Middlewares, compress=Compress, env=Env,
        max_empty_lines=MaxEmptyLines, max_keepalive=MaxKeepalive,
        max_request_line_length=MaxRequestLineLength,
        max_header_name_length=MaxHeaderNameLength,
        max_header_value_length=MaxHeaderValueLength, max_headers=MaxHeaders,
        onrequest=OnRequest, onresponse=OnResponse,
        timeout=Timeout, until=until(Timeout)},
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Taken from cowboy_protocol <END>
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    case socket_policy_server:read_policy_request(Socket, Transport) of
        {ok, policy} ->
            folsom_metrics:notify({list_to_existing_atom("socket_policy_" ++ port_string(Transport, Socket)), 1}),
            Transport:close(Socket);
        {ok, other, {ok, Buffer}} ->
            cowboy_protocol:parse_request(Buffer, State, erlang:size(Buffer));
        {ok, other, {error,timeout}} ->
            Transport:close(Socket);
        {ok, other, {error,closed}} ->
            ok;
        {ok, other, Other} ->
            lager:warning("data_broadcast_httpprotocol unhandled: ~p", [Other]),
            Transport:close(Socket)
    end.

port_string(Transport, Socket) ->
    {ok, {_IP, Port}} = Transport:sockname(Socket),
    erlang:integer_to_list(Port).
