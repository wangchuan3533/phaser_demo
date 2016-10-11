#ifndef _ROOM_H_
#define _ROOM_H_
#include <map>
#include <deque>
#include "common.h"
#include "tile_map.h"

class room {
    typedef std::deque<action_req_ptr> action_queue;
    
public:
    room(uint32_t room_id);
    virtual ~room();
    
    bool player_join(session *s, JoinRoomRes &join_room_res);
    bool player_leave(session *s);
    bool player_action(uint32_t player_id, action_req_ptr action_req, action_res_ptr action_res);
    bool update();
private:
    uint32_t _room_id;
    tile_map *_tile_map;
    std::map<uint32_t, player *> _players;
    std::map<uint32_t, action_queue> _actions_queues;
    uint32_t _player_id_seed;
    uint32_t _elapsed;
    uint64_t _start_time;
};
#endif
