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

-module(data_broadcast_incoming).

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
	StatsID = port_string(Transport, Socket),
	folsom_metrics:notify({list_to_existing_atom("client_count_" ++ StatsID), {inc,1}}),
    ok = ranch:accept_ack(ListenerPid),
    read_data(#state{socket=Socket, transport=Transport, id=ID, stats_id=StatsID}).

-spec read_data(#state{}) -> ok.
read_data(State=#state{socket=Socket, transport=Transport, id=ID, stats_id=StatsID}) ->
    case Transport:recv(Socket, 0, infinity) of
	{ok, Data} -> 
	    data_pusher:push(ID, Data),
	    read_data(State); 
	{error, _} -> 
		folsom_metrics:notify({list_to_existing_atom("client_count_" ++ StatsID), {dec,1}}),
		Transport:close(Socket),
		ok
    end.

port_string(Transport, Socket) ->
	{ok, {_IP, Port}} = Transport:sockname(Socket),
	erlang:integer_to_list(Port).
