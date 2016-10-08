-ifndef(ENTITY_PB_H).
-define(ENTITY_PB_H, true).
-record(entity, {
    id = erlang:error({required, id}),
    src = erlang:error({required, src}),
    dst = erlang:error({required, dst}),
    offset = erlang:error({required, offset})
}).
-endif.

-ifndef(JOINROOMREQ_PB_H).
-define(JOINROOMREQ_PB_H, true).
-record(joinroomreq, {
    room_id = erlang:error({required, room_id})
}).
-endif.

-ifndef(JOINROOMRES_PB_H).
-define(JOINROOMRES_PB_H, true).
-record(joinroomres, {
    room_id = erlang:error({required, room_id}),
    player_id = erlang:error({required, player_id}),
    entities = [],
    elapsed = erlang:error({required, elapsed})
}).
-endif.

-ifndef(LEAVEROOMREQ_PB_H).
-define(LEAVEROOMREQ_PB_H, true).
-record(leaveroomreq, {
    
}).
-endif.

-ifndef(LEAVEROOMRES_PB_H).
-define(LEAVEROOMRES_PB_H, true).
-record(leaveroomres, {
    
}).
-endif.

-ifndef(CHECKPOINTREQ_PB_H).
-define(CHECKPOINTREQ_PB_H, true).
-record(checkpointreq, {
    id = erlang:error({required, id}),
    src = erlang:error({required, src}),
    dst = erlang:error({required, dst}),
    offset = erlang:error({required, offset}),
    elapsed = erlang:error({required, elapsed}),
    latency = erlang:error({required, latency})
}).
-endif.

-ifndef(CHECKPOINTRES_PB_H).
-define(CHECKPOINTRES_PB_H, true).
-record(checkpointres, {
    id = erlang:error({required, id}),
    ret = erlang:error({required, ret}),
    elapsed = erlang:error({required, elapsed})
}).
-endif.

-ifndef(UPDATENTF_PB_H).
-define(UPDATENTF_PB_H, true).
-record(updatentf, {
    entities = [],
    elapsed = erlang:error({required, elapsed})
}).
-endif.

-ifndef(MESSAGE_PB_H).
-define(MESSAGE_PB_H, true).
-record(message, {
    type = erlang:error({required, type}),
    data = erlang:error({required, data})
}).
-endif.

