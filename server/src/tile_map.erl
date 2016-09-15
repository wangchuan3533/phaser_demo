-module(tile_map).
-include("common.hrl").

-export([
  load/2,
  is_wall/3,
  is_road/3,
  xy2index/3,
  index2xy/2,
  edge2xy/4,
  edge_distance/3,
  lookup_path/3,
  random_position/1,
  get_edge/2,
  random_edge/1,
  lookup_edge/3,
  test/0
]).

-record(tile_map, {
  width,
  height,
  node_count,
  index2node,
  node2index,
  directions,
  edge_count,
  edges,
  graph
}).

-record(graph_node, {
  left = -1,
  right = -1,
  up = -1,
  down = -1
}).

update_node(Node = #graph_node{}, Direction, Index) ->
  case Direction of
    ?LEFT ->
      Node#graph_node{left = Index};
    ?RIGHT ->
      Node#graph_node{right = Index};
    ?UP ->
      Node#graph_node{up = Index};
    ?DOWN ->
      Node#graph_node{down = Index};
    _Else ->
      Node
  end.
  
access_node(Node = #graph_node{}, Direction) ->
  case Direction of
    ?LEFT ->
      Node#graph_node.left;
    ?RIGHT ->
      Node#graph_node.right;
    ?UP ->
      Node#graph_node.up;
    ?DOWN ->
      Node#graph_node.down;
    _Else ->
      -1
  end.

load(PathFile, EdgeFile) ->
  case file:open(PathFile, [read, binary, raw]) of
    {ok, S1} ->
      {ok, <<Width:32/little, Height:32/little, NodeCount:32/little>>} = file:read(S1, 12),
      {ok, Node2Index} = file:read(S1, 4 * NodeCount),
      {ok, Directions} = file:read(S1, NodeCount * NodeCount),
      
      Index2Node = lists:foldl(fun(NodeId, Dict) ->
        <<Index:32/little>> = binary:part(Node2Index, NodeId * 4, 4),
        dict:store(Index, NodeId, Dict)
      end, dict:new(), lists:seq(0, NodeCount - 1)),
      
      case file:read_file(EdgeFile) of
        {ok, Edges} ->
          EdgeCount = byte_size(Edges) div 12,
          Graph = lists:foldl(fun(EdgeId, Dict) ->
            <<Src:32/little, Dst:32/little, Dir:32/little>> = binary:part(Edges, EdgeId * 12, 12),
            Node = case dict:find(Src, Dict) of
              {ok, Node1} ->
                update_node(Node1, Dir, Dst);
              error ->
                update_node(#graph_node{}, Dir, Dst)
            end,
            dict:store(Src, Node, Dict)
          end, dict:new(), lists:seq(0, EdgeCount - 1)),
          {ok, #tile_map{
            width = Width,
            height = Height,
            node_count = NodeCount,
            index2node = Index2Node,
            node2index = Node2Index,
            directions = Directions,
            edge_count = EdgeCount,
            edges = Edges,
            graph = Graph
          }};
        _Error ->
          error
      end;
    _Error ->
      error
  end.

is_wall(#tile_map{width = Width, height = Height}, X, Y) when X < 0; X >= Width; Y < 0; Y >= Height ->
  true;

is_wall(#tile_map{width = Width, index2node = Index2Node}, X, Y) ->
  Index = Y * Width + X,
  case dict:find(Index, Index2Node) of
    {ok, _NodeId} -> false;
    error -> true
  end.

is_road(TileMap, X, Y) -> not is_wall(TileMap, X, Y).

xy2index(#tile_map{width = Width}, X, Y) -> Y * Width + X.

index2xy(#tile_map{width = Width}, Index) -> {Index rem Width, Index div Width}.

edge2xy(TileMap, Src, Dst, Offset) ->
  {X1, Y1} = index2xy(TileMap, Src),
  {X2, Y2} = index2xy(TileMap, Dst),
  
  if
    X2 > X1 ->
      {X1 + Offset, Y1};
    X2 < X1 ->
      {X1 - Offset, Y1};
    Y2 > Y1 ->
      {X1, Y1 + Offset};
    Y2 < Y1 ->
      {X1, Y1 - Offset};
    true ->
      {X1, Y1}
  end.

edge_distance(TileMap, Src, Dst) ->
  {X1, Y1} = index2xy(TileMap, Src),
  {X2, Y2} = index2xy(TileMap, Dst),
  abs(X2 - X1) + abs(Y2 - Y1).

lookup_path(#tile_map{node_count = NodeCount, index2node = Index2Node, directions = Directions}, Index1, Index2) ->
  {ok, NodeId1} = dict:fetch(Index1, Index2Node),
  {ok, NodeId2} = dict:fetch(Index2, Index2Node),
  Offset = NodeId1 * NodeCount + NodeId2,
  <<Direction:8/little>> = binary:part(Directions, Offset, 1),
  Direction.

random_position(TileMap = #tile_map{node_count = NodeCount, node2index = Node2Index}) ->
  _ = rand:uniform(NodeCount),
  NodeId = rand:uniform(NodeCount) - 1,
  <<Index:32/little>> = binary:part(Node2Index, NodeId, 4),
  index2xy(TileMap, Index).

get_edge(#tile_map{edges = Edges}, Index) ->
  <<Src:32/little, Dst:32/little, Direction:32/little>> = binary:part(Edges, Index * 12, 12),
  {Src, Dst, Direction}.

random_edge(TileMap = #tile_map{edge_count = EdgeCount}) ->
  _ = rand:uniform(EdgeCount),
  Index = rand:uniform(EdgeCount) - 1,
  get_edge(TileMap, Index).
  
lookup_edge(#tile_map{graph = Graph}, Src, Direction) ->
  case dict:find(Src, Graph) of
    {ok, Node} -> access_node(Node, Direction);
    error -> -1
  end.

test() ->
  {ok, TileMap} = load(<<"../../../shared/map/64.tmx.path.data">>, <<"../../../shared/map/64.tmx.edge.data">>),
  true = is_road(TileMap, 1, 0),
  false = is_wall(TileMap, 1, 0),
  1 = xy2index(TileMap, 1, 0),
  {1, 0} = index2xy(TileMap, 1),
  ok.
