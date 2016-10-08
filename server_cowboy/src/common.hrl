-include("messages_pb.hrl").
%% types
-type entity() :: #entity{}.


-define(LEFT, 1).
-define(RIGHT, 2).
-define(UP, 3).
-define(DOWN, 4).
-define(BASE_SPEED, 6).
-define(TICK, 15).
-define(LATENCY_WINDOW_SIZE, 64).
