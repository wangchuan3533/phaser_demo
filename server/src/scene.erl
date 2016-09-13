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
  actions :: queue:queue(#actionreq{})
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

opposite_direction(?LEFT) -> ?RIGHT;
opposite_direction(?RIGHT) -> ?LEFT;
opposite_direction(?UP) -> ?DOWN;
opposite_direction(?DOWN) -> ?UP;
opposite_direction(_Other) -> -1.

broadcast(Players, Msg) ->
  lists:foreach(fun(#player_state{pid = Pid}) ->
    Pid ! Msg
  end, Players).
  
update_entity(Entity = #entity{src = Src, dst = Dst, offset = Offset, direction = Direction, next_direction = NextDirection}, Interval, TileMap) ->
  if Src >= 0, Dst >= 0 ->
    MaxOffset = tile_map:edge_distance(TileMap, Src, Dst),
    if Offset =< MaxOffset ->
      Delta = ?BASE_SPEED * Interval / 1000,
      Offset1 = Offset + Delta,
      if Offset1 >= MaxOffset ->
        Dst1 = tile_map:lookup_edge(TileMap, Dst, NextDirection),
        Dst2 = tile_map:lookup_edge(TileMap, Dst, Direction),
        if
          Dst1 >= 0 ->
            Entity#entity{src = Dst, dst = Dst1, offset = Offset1 - MaxOffset, direction = NextDirection, next_direction = -1};
          Dst2 >= 0 ->
            Entity#entity{src = Dst, dst = Dst2, offset = Offset1 - MaxOffset};
          true ->
            Entity#entity{src = Dst, dst = -1, offset = 0, direction = -1, next_direction = -1}
        end;
      true ->
        Entity#entity{offset = Offset1}
      end;
    true ->
      Entity
    end;
  true ->
    Entity
  end.

control_entity(Entity = #entity{src = Src, dst = Dst, offset = Offset, direction = CurrentDirection, next_direction = NextDirection}, Direction, TileMap) ->
  OppositeDirection = opposite_direction(CurrentDirection),
  if
    Dst == -1 ->
      Dst1 = tile_map:lookup_edge(TileMap, Src, Direction),
      if Dst1 >= 0 ->
        Entity#entity{dst = Dst1, offset = 0, direction = Direction, next_direction = -1};
      true ->
        Entity
      end;
    Direction == OppositeDirection ->
      MaxOffset = tile_map:edge_distance(TileMap, Src, Dst),
      Entity#entity{src = Dst, dst = Src, offset = MaxOffset - Offset, direction = Direction, next_direction = -1};
    Direction /= CurrentDirection, Direction /= NextDirection ->
      Entity#entity{next_direction = Direction};
    true ->
      Entity
  end.

%% gen_server.

init([]) ->
  {ok, TileMap} = tile_map:load(<<"../../../shared/map/64.tmx.path.data">>, <<"../../../shared/map/64.tmx.edge.data">>),
  Timer = erlang:send_after(?TICK, self(), tick),
  {ok, #state{tile_map = TileMap, player_id_seed = 1, timer = Timer, players = [], tick = 0, now = 16#ffffffff band erlang:system_time(milli_seconds)}}.

handle_call(#joinroomreq{}, {Pid, _Tag}, State = #state{tile_map = TileMap, player_id_seed = PlayerId, players = Players}) ->
  ok = io:format("join from ~p~n", [Pid]),
  {Src, _Dst, _Direction} = tile_map:random_edge(TileMap),
  Entity = #entity{id = PlayerId, src = Src, dst = -1, offset = 0, direction = -1, next_direction = -1},
  Player = #player_state{pid = Pid, player_id = PlayerId, entity = Entity, actions = queue:new()},
  {reply, #joinroomres{room_id = 1, player_id = PlayerId, entity = Entity}, State#state{player_id_seed = PlayerId + 1, players = [Player | Players]}};
  
%handle_call(Action = #actionreq{direction = Direction, ts = TsClient}, {Pid, _Tag}, State = #state{now = Now, players = Players, tile_map = TileMap}) ->
%  Player = lists:keyfind(Pid, #player_state.pid, Players),
%  ok = io:format("player: ~p direction:~p ~n", [Player#player_state.player_id, Direction]),
%  #player_state{entity = Entity, actions = Actions} = Player,
%  NewActions = queue:in(Action, Actions),
%  NewEntity = control_entity(Entity, Direction, TileMap),
%  NewPlayer = Player#player_state{entity = NewEntity, actions = NewActions},
%  NewPlayers = lists:keyreplace(Pid, #player_state.pid, Players, NewPlayer),
%  TsServer = Now band 16#ffffffff,
%  Latency = TsServer - TsClient,
%  ok = io:format("~p~n", [Latency]),
%  {reply, #actionres{ts = TsServer}, State#state{players = NewPlayers}};
  
handle_call(Action = #actionreq{}, {Pid, _Tag}, State = #state{now = Now, players = Players}) ->
  Player = lists:keyfind(Pid, #player_state.pid, Players),
  #player_state{actions = Actions} = Player,
  Actions1 = queue:in(Action, Actions),
  Player1 = Player#player_state{actions = Actions1},
  Players1 = lists:keyreplace(Pid, #player_state.pid, Players, Player1),
  {reply, #actionres{ts = Now}, State#state{players = Players1}};
  
handle_call(_Request, _From, State) ->
  {reply, #actionres{}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(tick, State = #state{tile_map = TileMap, timer = OldTimer, players = Players, now = Timestamp, tick = Tick}) ->
  _Result = erlang:cancel_timer(OldTimer),
  % update
  Tick1 = Tick + 1,
  Timestamp1 = 16#ffffffff band erlang:system_time(milli_seconds),
  DT = Timestamp1 - Timestamp,
  Players1 = lists:map(fun(Player = #player_state{entity = Entity, actions = Actions}) ->
    case queue:peek(Actions) of
      {value, #actionreq{direction = Direction, ts = Ts}} when Timestamp - Ts > 200 ->
        Latency = Timestamp - Ts,
        ok = io:format("latency: ~p~n", [Latency]),
        Entity1 = control_entity(Entity, Direction, TileMap),
        Entity2 = update_entity(Entity1, DT, TileMap),
        Actions1 = queue:drop(Actions),
        Player#player_state{entity = Entity2, actions = Actions1};
      _Other ->
        Entity1 = update_entity(Entity, DT, TileMap),
        Player#player_state{entity = Entity1}
    end
  end, Players),
  Entities = lists:map(fun(#player_state{entity = Entity}) -> Entity end, Players1),
  ok = case Tick rem 2 of
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
