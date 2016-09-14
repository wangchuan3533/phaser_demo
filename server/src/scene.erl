-module(scene).
-behaviour(gen_server).

-include("common.hrl").
%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(player_state, {
  pid :: pid(),
  player_id :: integer(),
  entity :: entity(),
  checkpoints :: queue:queue(#checkpointreq{}),
  avg_latency :: integer()
}).

-record(state, {
  tick :: integer(),
  now :: integer(),
  tile_map,
  timer :: timer:tref(),
  player_id_seed :: integer(),
  players :: [#player_state{}]
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% internal

broadcast(Players, Msg) ->
  lists:foreach(fun(#player_state{pid = Pid}) ->
    Pid ! Msg
  end, Players).
  
update_entity(Entity = #entity{offset = Offset}, Interval, _TileMap) ->
  Offset1 = Offset + ?BASE_SPEED * Interval * 16#10000 div 1000,
  Entity#entity{offset = Offset1}.
%% gen_server.

init([]) ->
  {ok, TileMap} = tile_map:load(<<"../../../shared/map/6.tmx.path.data">>, <<"../../../shared/map/6.tmx.edge.data">>),
  Timer = erlang:send_after(?TICK, self(), tick),
  {ok, #state{tile_map = TileMap, player_id_seed = 1, timer = Timer, players = [], tick = 0, now = 16#ffffffff band erlang:system_time(milli_seconds)}}.

handle_call(#joinroomreq{room_id = _RoomId, ts = Ts}, {Pid, _Tag}, State = #state{now = Now, tile_map = TileMap, player_id_seed = PlayerId, players = Players}) ->
  ok = io:format("join from ~p~n", [Pid]),
  {Src, _Dst, _Direction} = tile_map:random_edge(TileMap),
  Entity = #entity{id = PlayerId, src = Src, dst = -1, offset = 0},
  Player = #player_state{pid = Pid, player_id = PlayerId, entity = Entity, checkpoints = queue:new(), avg_latency = Now - Ts},
  {reply, #joinroomres{room_id = 1, player_id = PlayerId, entity = Entity}, State#state{player_id_seed = PlayerId + 1, players = [Player | Players]}};
  
handle_call(Checkpoint = #checkpointreq{ts = Ts}, {Pid, _Tag}, State = #state{now = Now, players = Players}) ->
  Player = lists:keyfind(Pid, #player_state.pid, Players),
  Latency = Now - Ts,
  #player_state{checkpoints = Checkpoints, avg_latency = AvgLantency} = Player,
  AvgLantency1 = AvgLantency + (Latency - AvgLantency) / ?LATENCY_WINDOW_SIZE,
  ok = io:format("avg_latency ~p~n", [AvgLantency1]),
  Checkpoints1 = queue:in(Checkpoint, Checkpoints),
  Player1 = Player#player_state{checkpoints = Checkpoints1, avg_latency = AvgLantency1},
  Players1 = lists:keyreplace(Pid, #player_state.pid, Players, Player1),
  {reply, #checkpointres{ret = 1, ts = Now}, State#state{players = Players1}}.
  
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(tick, State = #state{tile_map = TileMap, timer = OldTimer, players = Players, now = Timestamp, tick = Tick}) ->
  _Result = erlang:cancel_timer(OldTimer),
  % update
  Tick1 = Tick + 1,
  Timestamp1 = 16#ffffffff band erlang:system_time(milli_seconds),
  DT = Timestamp1 - Timestamp,
  Players1 = lists:map(fun(Player = #player_state{entity = Entity, checkpoints = Checkpoints, avg_latency = AvgLantency}) ->
    case queue:peek(Checkpoints) of
      {value, #checkpointreq{ts = Ts, src = Src, dst = Dst, offset = Offset}} when Timestamp - Ts >= AvgLantency ->
        Latency = Timestamp - Ts,
        ok = io:format("latency ~p~n", [Latency]),
        Entity1 = Entity#entity{src = Src, dst = Dst, offset = Offset},
        Entity2 = update_entity(Entity1, DT, TileMap),
        Checkpoints1 = queue:drop(Checkpoints),
        Player#player_state{entity = Entity2, checkpoints = Checkpoints1};
      _Other ->
        Entity1 = update_entity(Entity, DT, TileMap),
        Player#player_state{entity = Entity1}
    end
  end, Players),
  Entities = lists:map(fun(#player_state{entity = Entity}) -> Entity end, Players1),
  ok = case Tick rem 1 of
    0 -> broadcast(Players, #updatentf{entities = Entities});
    _ -> ok
  end,
  Timer = erlang:send_after(?TICK, self(), tick),
  {noreply, State#state{timer = Timer, now = Timestamp1, players = Players1, tick = Tick1}};
  
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
