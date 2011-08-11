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

-export([start_link/3]). 
-export([init/3]). 

-record(state, {
	socket :: inet:socket(),
	transport :: module()
}).

-spec start_link(inet:socket(), module(), any()) -> {ok, pid()}.
start_link(Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Socket, Transport, Opts]),
	{ok, Pid}.

-spec init(inet:socket(), module(), any()) -> ok.
init(Socket, Transport, _Opts) ->
    data_pusher:subscribe(),
    data_loop(#state{socket=Socket, transport=Transport}).

-spec data_loop(#state{}) -> ok.
data_loop(State=#state{socket=Socket, transport=Transport}) ->
    receive
	{send, SendData} ->
	    case Transport:send(Socket, SendData) of
		ok -> data_loop(State);
		{error, _Reason} -> 
		    data_pusher:unsubscribe(), ok
	    end
    after
	1000 ->
	    %% drop data the data receiver sends us
	    case Transport:recv(Socket, 0, 0) of
		{ok, _Data} -> data_loop(State);
		{error, timeout} -> data_loop(State);
		{error, _Reason} -> 
		    data_pusher:unsubscribe(), ok
	    end
    end.
