-module(ws_handler).
-behaviour(cowboy_websocket_handler).
-include("common.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
  player :: pid()
}).

init(_, _, _) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
  Req2 = cowboy_req:compact(Req),
  {ok, Player} = player:start_link({self()}),
  {ok, Req2, #state{player = Player}}.

websocket_handle({text, Data}, Req, State) ->
  {reply, {text, Data}, Req, State};

websocket_handle({binary, Data}, Req, State = #state{player = Player}) ->
  Request = protocol:decode(Data),
  % ok = io:format("[req] ~p~n", [Request]),
  Response = gen_server:call(Player, Request),
  % ok = io:format("[res] ~p~n", [Response]),
  {reply, {binary, protocol:encode(Response)}, Req, State};
  
websocket_handle(_Frame, Req, State) ->
  {ok, Req, State}.

websocket_info(Ntf = #updatentf{}, Req, State) ->
  %% ok = io:format("[ntf] ~p~n", [Ntf]),
  {reply, {binary, protocol:encode(Ntf)}, Req, State};
  
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  % TODO: kill player
  ok.
