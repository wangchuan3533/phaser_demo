-ifndef(ENTITY_PB_H).
-define(ENTITY_PB_H, true).
-record(entity, {
    id = erlang:error({required, id}),
    src = erlang:error({required, src}),
    dst = erlang:error({required, dst}),
    offset = erlang:error({required, offset}),
    direction = erlang:error({required, direction}),
    next_direction = erlang:error({required, next_direction})
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
    entity = erlang:error({required, entity})
}).
-endif.

-ifndef(ACTIONREQ_PB_H).
-define(ACTIONREQ_PB_H, true).
-record(actionreq, {
    direction = erlang:error({required, direction}),
    ts = erlang:error({required, ts})
}).
-endif.

-ifndef(ACTIONRES_PB_H).
-define(ACTIONRES_PB_H, true).
-record(actionres, {
    ts = erlang:error({required, ts})
}).
-endif.

-ifndef(UPDATENTF_PB_H).
-define(UPDATENTF_PB_H, true).
-record(updatentf, {
    entities = []
}).
-endif.

-ifndef(MESSAGE_PB_H).
-define(MESSAGE_PB_H, true).
-record(message, {
    type = erlang:error({required, type}),
    data = erlang:error({required, data})
}).
-endif.

