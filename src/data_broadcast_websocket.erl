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

-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).


init({_Any, http}, Req, _Opts) ->
    case cowboy_req:header(<<"upgrade">>, Req) of
        {undefined, Req2} -> {ok, Req2, undefined_state};
        _ -> {upgrade, protocol, cowboy_websocket}
    end.


handle(Req, State) ->
    {Path, PathReq} = cowboy_req:path(Req),
    case Path of
        <<"/crossdomain.xml">> ->
            {ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/x-cross-domain-policy">>}], <<"<?xml version=\"1.0\"?><cross-domain-policy><site-control permitted-cross-domain-policies=\"all\"/><allow-access-from domain=\"*\" to-ports=\"80,8080,5002,5013,6002,6013,7002,7013,8002,8013,9002,9013\"/></cross-domain-policy>">>, PathReq),
            {ok, Req2, State};
       _ ->
            {ok, Req3} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], <<"This space intentionally left blank.">>, PathReq),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

-record(state, {
      id :: integer()
     }).

websocket_init(_Any, Req, [ID]) ->
    data_pusher:subscribe(ID),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{id=ID}}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "You said: ", Msg/binary >>}, Req, State, hibernate};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_info({send, Data}, Req, State) ->
    {reply, {binary, Data}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(Reason, _Req, #state{id=ID}) ->
    data_pusher:unsubscribe(ID),
    lager:warning("websocket_terminate received: ~p", [Reason]),
    ok.
