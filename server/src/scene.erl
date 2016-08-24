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

-record(state, {
  now,
  map_data,
  timer,
  player_id_seed,
  players
}).

-record(player_state, {
  pid,
  player_id,
  entity
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
  
update_entity(Entity = #entity{src = Src, dst = Dst, offset = Offset, direction = Direction, next_direction = NextDirection}, Interval, MapData) ->
  if Src >= 0, Dst >= 0 ->
    MaxOffset = tile_map:edge_distance(MapData, Src, Dst),
    if Offset =< MaxOffset ->
      Delta = ?BASE_SPEED * Interval / 1000,
      Offset1 = Offset + Delta,
      if Offset1 >= MaxOffset ->
        Dst1 = tile_map:lookup_edge(MapData, Dst, NextDirection),
        Dst2 = tile_map:lookup_edge(MapData, Dst, Direction),
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

control_entity(Entity = #entity{src = Src, dst = Dst, offset = Offset, direction = CurrentDirection, next_direction = NextDirection}, Direction, MapData) ->
  OppositeDirection = opposite_direction(CurrentDirection),
  if
    Dst == -1 ->
      Dst1 = tile_map:lookup_edge(MapData, Src, Direction),
      if Dst1 >= 0 ->
        Entity#entity{dst = Dst1, offset = 0, direction = Direction, next_direction = -1};
      true ->
        Entity
      end;
    Direction == OppositeDirection ->
      MaxOffset = tile_map:edge_distance(MapData, Src, Dst),
      Entity#entity{src = Dst, dst = Src, offset = MaxOffset - Offset, direction = Direction, next_direction = -1};
    Direction /= CurrentDirection, Direction /= NextDirection ->
      Entity#entity{next_direction = Direction};
    true ->
      Entity
  end.

%% gen_server.

init([]) ->
  {ok, MapData} = tile_map:load(<<"../../../shared/map/64.tmx.path.data">>, <<"../../../shared/map/64.tmx.edge.data">>),
  Timer = erlang:send_after(?TICK, self(), tick),
  {ok, #state{map_data = MapData, player_id_seed = 1, timer = Timer, players = [], now = erlang:system_time(milli_seconds)}}.

handle_call(#joinroomreq{}, {Pid, _Tag}, State = #state{map_data = MapData, player_id_seed = PlayerId, players = Players}) ->
  ok = io:format("join from ~p~n", [Pid]),
  {Src, _Dst, _Direction} = tile_map:random_edge(MapData),
  Entity = #entity{id = PlayerId, src = Src, dst = -1, offset = 0, direction = -1, next_direction = -1},
  Player = #player_state{pid = Pid, player_id = PlayerId, entity = Entity},
  {reply, #joinroomres{room_id = 1, player_id = PlayerId, entity = Entity}, State#state{player_id_seed = PlayerId + 1, players = [Player | Players]}};
  
handle_call(#actionreq{direction = Direction}, {Pid, _Tag}, State = #state{players = Players, map_data = MapData}) ->
  Player = lists:keyfind(Pid, #player_state.pid, Players),
  ok = io:format("player: ~p direction:~p ~n", [Player#player_state.player_id, Direction]),
  Entity = Player#player_state.entity,
  NewEntity = control_entity(Entity, Direction, MapData),
  NewPlayer = Player#player_state{entity = NewEntity},
  NewPlayers = lists:keyreplace(Pid, #player_state.pid, Players, NewPlayer),
  {reply, #actionres{}, State#state{players = NewPlayers}};
  
handle_call(_Request, _From, State) ->
  {reply, #actionres{}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(tick, State = #state{map_data = MapData, timer = OldTimer, players = Players, now = OldTimestamp}) ->
  _Result = erlang:cancel_timer(OldTimer),
  % update
  NewTimestamp = erlang:system_time(milli_seconds),
  DT = NewTimestamp - OldTimestamp,
  NewPlayers = lists:map(fun(Player = #player_state{entity = Entity}) ->
    NewEntity = update_entity(Entity, DT, MapData),
    Player#player_state{entity = NewEntity}
  end, Players),
  Entities = lists:map(fun(#player_state{entity = Entity}) -> Entity end, NewPlayers),
  ok = broadcast(Players, #updatentf{entities = Entities}),
  Timer = erlang:send_after(?TICK, self(), tick),
  {noreply, State#state{timer = Timer, now = NewTimestamp, players = NewPlayers}};
  
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
