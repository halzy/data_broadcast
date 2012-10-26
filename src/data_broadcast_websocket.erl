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

-module(data_broadcast_websocket).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, Opts) ->
    case cowboy_req:header(<<"upgrade">>, Req) of
    	{undefined, Req2} -> {ok, Req2, undefined_state};
    	_Other -> 
            {upgrade, protocol, cowboy_websocket}
    end.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}],
%% HTML code taken from misultin's example file.
<<"<html>
<head>
<script type=\"text/javascript\">
function addStatus(text){
	var date = new Date();
	document.getElementById('status').innerHTML
		= document.getElementById('status').innerHTML
		+ date + \": \" + text + \"<br/>\";
}
function ready(){
	if (\"MozWebSocket\" in window) {
		WebSocket = MozWebSocket;
	}
	if (\"WebSocket\" in window) {
		// browser supports websockets
		var ws = new WebSocket(\"ws://localhost:8002/\");
		ws.onopen = function() {
			// websocket is connected
			addStatus(\"websocket connected!\");
		};
		ws.onmessage = function (evt) {
			var receivedMsg = evt.data;
			addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
		};
        ws.onerror = function (evt) {
            addStatus(\"error: '\" + evt + \"'\")
        };
		ws.onclose = function() {
			// websocket was closed
			addStatus(\"websocket was closed\");
		};
	} else {
		// browser does not support websockets
		addStatus(\"sorry, your browser does not support websockets.\");
	}
}
</script>
</head>
<body onload=\"ready();\">
		<div id=\"status\"></div>
</body>
		</html>">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

-record(state, {
      id :: integer()
     }).

websocket_init(_Any, Req, [ID]) ->
    data_pusher:subscribe(ID),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{id=ID}, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "You said: ", Msg/binary >>}, Req, State, hibernate};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_info({send, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, #state{id=ID}) ->
    data_pusher:unsubscribe(ID),
    ok.
