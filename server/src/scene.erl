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
  latency :: integer()
}).

-record(state, {
  tick :: integer(),
  start_time :: integer(),
  elapsed :: integer(),
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
  
sync_entity(Entity = #entity{src = Src1, dst = Dst1, offset = Offset1}, #checkpointreq{src = Src2, dst = Dst2, offset = Offset2}, TileMap) ->
  Total = tile_map:edge_distance(TileMap, Src1, Dst1) * 16#10000,
  ok = if
    Src1 == Src2, Dst1 == Dst2 ->
    Delta = (Offset2 - Offset1) / 16#10000,
    ok = io:format("[sync] same src, same dst delta = ~p~n", [Delta]);
  Src1 == Dst2, Dst1 == Src2 ->
    Delta = (Total - Offset1 - Offset2) / 16#10000,
    ok = io:format("[sync] opposite src, dst delta = ~p~n", [Delta]);
  Dst1 == Src2 ->
    Delta = (Offset1 - Total - Offset2) / 16#10000,
    ok = io:format("[sync] branch, delta = ~p~n", [Delta]);
  true ->
    ok
  end,
  Entity#entity{src = Src2, dst = Dst2, offset = Offset2}.
  
update_entity(Entity = #entity{offset = Offset}, Interval, _TileMap) ->
  Offset1 = Offset + ?BASE_SPEED * Interval * 16#10000 div 1000,
  Entity#entity{offset = Offset1}.
%% gen_server.

init([]) ->
  {ok, TileMap} = tile_map:load(<<"../../../shared/map/6.tmx.path.data">>, <<"../../../shared/map/6.tmx.edge.data">>),
  Timer = erlang:send_after(?TICK, self(), tick),
  StartTime = erlang:system_time(milli_seconds),
  ok = io:format("start_time ~p~n", [StartTime]),
  {ok, #state{tile_map = TileMap, player_id_seed = 1, timer = Timer, players = [], tick = 0, start_time = StartTime, elapsed = 0}}.

handle_call(#joinroomreq{room_id = _RoomId}, {Pid, _Tag}, State = #state{start_time = StartTime, tile_map = TileMap, player_id_seed = PlayerId, players = Players}) ->
  ok = io:format("join from ~p~n", [Pid]),
  Elapsed = erlang:system_time(milli_seconds) - StartTime,
  {Src, _Dst, _Direction} = tile_map:random_edge(TileMap),
  Entity = #entity{id = PlayerId, src = Src, dst = -1, offset = 0},
  Player = #player_state{pid = Pid, player_id = PlayerId, entity = Entity, checkpoints = queue:new(), latency = 0},
  Players1 = [Player | Players],
  Entities = lists:map(fun(#player_state{entity = E}) -> E end, Players1),
  {reply, #joinroomres{room_id = 1, player_id = PlayerId, entities = Entities, elapsed = Elapsed}, State#state{player_id_seed = PlayerId + 1, players = [Player | Players]}};
  
handle_call(#leaveroomreq{}, {Pid, _Tag}, State = #state{players = Players}) ->
  Player = lists:keyfind(Pid, #player_state.pid, Players),
  ok = io:format("player ~p leaved ~n", [Player#player_state.player_id]),
  Players1 = lists:keydelete(Pid, #player_state.pid, Players),
  {reply, ok, State#state{players = Players1}};
  
handle_call(Checkpoint = #checkpointreq{id = Id, latency = Latency}, {Pid, _Tag}, State = #state{start_time = StartTime, players = Players}) ->
  Elapsed = erlang:system_time(milli_seconds) - StartTime,
  Player = lists:keyfind(Pid, #player_state.pid, Players),
  #player_state{checkpoints = Checkpoints} = Player,
  % ok = io:format("[latency] avg ~p~n", [Latency]),
  Checkpoints1 = queue:in(Checkpoint, Checkpoints),
  Player1 = Player#player_state{checkpoints = Checkpoints1, latency = Latency},
  Players1 = lists:keyreplace(Pid, #player_state.pid, Players, Player1),
  {reply, #checkpointres{ret = 1, elapsed = Elapsed, id = Id}, State#state{players = Players1}}.
  
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(tick, State = #state{tile_map = TileMap, timer = OldTimer, players = Players, elapsed = Elapsed, tick = Tick, start_time = StartTime}) ->
  _Result = erlang:cancel_timer(OldTimer),
  % update
  Tick1 = Tick + 1,
  Elapsed1 = erlang:system_time(milli_seconds) - StartTime,
  DT = Elapsed1 - Elapsed,
  Players1 = lists:map(fun(Player = #player_state{entity = Entity, checkpoints = Checkpoints, latency = Latency}) ->
    case queue:peek(Checkpoints) of
      {value, Checkpoint = #checkpointreq{elapsed = ClientElapsed}} when Elapsed - ClientElapsed >= Latency / 2 ->
        ok = io:format("[latency] tick latency: ~p, diff: ~p~n", [Latency, Elapsed - ClientElapsed]),
        Entity1 = sync_entity(Entity, Checkpoint, TileMap),
        Entity2 = update_entity(Entity1, DT, TileMap),
        Checkpoints1 = queue:drop(Checkpoints),
        Player#player_state{entity = Entity2, checkpoints = Checkpoints1};
      _Other ->
        Entity1 = update_entity(Entity, DT, TileMap),
        Player#player_state{entity = Entity1}
    end
  end, Players),
  Entities = lists:map(fun(#player_state{entity = Entity}) -> Entity end, Players1),
  ok = case Tick rem 4 of
    0 -> broadcast(Players, #updatentf{entities = Entities, elapsed = Elapsed1});
    _ -> ok
  end,
  Timer = erlang:send_after(?TICK, self(), tick),
  {noreply, State#state{timer = Timer, elapsed = Elapsed1, players = Players1, tick = Tick1}};
  
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
