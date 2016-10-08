-module(player).
-behaviour(gen_server).

-include("common.hrl").

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  ws :: pid(),
  scene :: pid()
}).

%% API.

-spec start_link({pid()}) -> {ok, pid()}.
start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).

%% gen_server.

init(Opts) ->
  % TODO: dispatch scene
  {Ws} = Opts,
  Scene = scene,
  {ok, #state{ws = Ws, scene = Scene}}.

handle_call(Req, {Ws, _Tag}, State = #state{ws = Ws, scene = Scene}) ->
  Res = gen_server:call(Scene, Req),
  {reply, Res, State};
  
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Ntf = #updatentf{}, State = #state{ws = Ws}) ->
  Ws ! Ntf,
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
