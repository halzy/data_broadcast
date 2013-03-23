%% Copyright (c) 2011, Benjamin Halsted <benhalsted@sportvision.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(data_broadcast_outgoing).

-export([start_link/4]). 
-export([init/4]). 

-record(state, {
	socket :: inet:socket(),
	transport :: module(),
    id :: integer(),
    stats_id :: string()
}).

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, [ID]) ->
    ok = ranch:accept_ack(ListenerPid),
    case socket_policy_server:read_policy_request(Socket, Transport) of
        {ok, policy} ->
            folsom_metrics:notify({list_to_existing_atom("socket_policy_" ++ port_string(Transport, Socket)), {inc, 1}}),
            Transport:close(Socket);
        {ok, other, _} ->
            StatsID = port_string(Transport, Socket),
            folsom_metrics:notify({list_to_existing_atom("client_count_" ++ StatsID), {inc,1}}),
            data_pusher:subscribe(ID),
            send_loop(#state{socket=Socket, transport=Transport, id=ID, stats_id=StatsID})
    end.

-spec send_loop(#state{}) -> ok.
send_loop(State=#state{socket=Socket, transport=Transport, id=ID, stats_id=StatsID}) ->
    receive
    	{send, SendData} ->
    	    case Transport:send(Socket, SendData) of
        		ok -> read_loop(State);
                {error, closed} -> 
                    data_pusher:unsubscribe(ID), 
                    folsom_metrics:notify({list_to_existing_atom("client_count_" ++ StatsID), {dec,1}}),
                    ok;
        		{error, Error} -> 
        		    data_pusher:unsubscribe(ID), 
                    folsom_metrics:notify({list_to_existing_atom("client_count_" ++ StatsID), {dec,1}}),
                    folsom_metrics:notify({list_to_existing_atom("errors_" ++ StatsID), {inc, 1}}),
                    lager:error("send outgoing ~p: ~p~n", [StatsID, Error]),
                    ok
    	    end
    after 1000 ->
        read_loop(State)
    end.

-spec read_loop(#state{}) -> ok.
read_loop(State=#state{socket=Socket, transport=Transport, id=ID, stats_id=StatsID}) ->
    %% drop data the data receiver sends us
    case Transport:recv(Socket, 0, 0) of
        {ok, _Data} -> send_loop(State);
        {error, timeout} -> send_loop(State);
        {error, closed} -> 
            data_pusher:unsubscribe(ID),
            folsom_metrics:notify({list_to_existing_atom("client_count_" ++ StatsID), {dec,1}}),
            ok;
        {error, Error} -> 
            data_pusher:unsubscribe(ID),
            folsom_metrics:notify({list_to_existing_atom("client_count_" ++ StatsID), {dec,1}}),
            folsom_metrics:notify({list_to_existing_atom("errors_" ++ StatsID), {inc, 1}}),
            lager:error("read outgoing ~p: ~p~n", [StatsID, Error]),
            ok
    end.

port_string(Transport, Socket) ->
    Something = Transport:sockname(Socket),
    {ok, {_IP, Port}} = Something,
    erlang:integer_to_list(Port).
