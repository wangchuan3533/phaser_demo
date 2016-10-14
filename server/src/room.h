#ifndef _ROOM_H_
#define _ROOM_H_
#include <map>
#include <deque>
#include "common.h"
#include "tile_map.h"

class room {
    typedef std::deque<action_req_shared_ptr> action_queue;
    
public:
    room(uint32_t room_id);
    virtual ~room();
    
    bool player_join(session *s, demo::protocol::JoinRoomRes *join_room_res);
    bool player_leave(session *s);
    bool player_action(uint32_t player_id, action_req_shared_ptr action_req, demo::protocol::ActionRes *action_res);
    bool update();
    
public:
    static room *create_room()
    {
        room *r = new room(_room_id_seed);
        _rooms[_room_id_seed++] = r;
        return r;
    }

    static room *get_room(uint32_t room_id)
    {
        auto it = _rooms.find(room_id);
        if (it == _rooms.end()) return NULL;
        return it->second;
    }
    
    static void update_all()
    {
        for (auto & it : _rooms) {
            it.second->update();
        }
    }
    
private:
    uint32_t _room_id;
    tile_map *_tile_map;
    std::map<uint32_t, player *> _players;
    std::map<uint32_t, action_queue> _actions_queues;
    uint32_t _player_id_seed;
    uint32_t _elapsed;
    uint64_t _start_time;
    
    static std::map<uint32_t, room *> _rooms;
    static uint32_t _room_id_seed;
};
#endif
