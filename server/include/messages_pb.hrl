-ifndef(ENTITY_PB_H).
-define(ENTITY_PB_H, true).
-record(entity, {
    id = erlang:error({required, id}),
    src = erlang:error({required, src}),
    dst = erlang:error({required, dst}),
    offset = erlang:error({required, offset})
}).
-endif.

-ifndef(CHECKPOINTREQ_PB_H).
-define(CHECKPOINTREQ_PB_H, true).
-record(checkpointreq, {
    id = erlang:error({required, id}),
    src = erlang:error({required, src}),
    dst = erlang:error({required, dst}),
    offset = erlang:error({required, offset}),
    ts = erlang:error({required, ts})
}).
-endif.

-ifndef(CHECKPOINTRES_PB_H).
-define(CHECKPOINTRES_PB_H, true).
-record(checkpointres, {
    ret = erlang:error({required, ret}),
    ts = erlang:error({required, ts})
}).
-endif.

-ifndef(JOINROOMREQ_PB_H).
-define(JOINROOMREQ_PB_H, true).
-record(joinroomreq, {
    room_id = erlang:error({required, room_id}),
    ts = erlang:error({required, ts})
}).
-endif.

-ifndef(JOINROOMRES_PB_H).
-define(JOINROOMRES_PB_H, true).
-record(joinroomres, {
    room_id = erlang:error({required, room_id}),
    player_id = erlang:error({required, player_id}),
    entities = [],
    ts = erlang:error({required, ts})
}).
-endif.

-ifndef(UPDATENTF_PB_H).
-define(UPDATENTF_PB_H, true).
-record(updatentf, {
    entities = [],
    ts = erlang:error({required, ts})
}).
-endif.

-ifndef(MESSAGE_PB_H).
-define(MESSAGE_PB_H, true).
-record(message, {
    type = erlang:error({required, type}),
    data = erlang:error({required, data})
}).
-endif.

