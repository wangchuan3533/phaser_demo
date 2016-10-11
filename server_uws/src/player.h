#ifndef _PLAYER_H_
#define _PLAYER_H_
#include "common.h"
#include <queue>

class player {
public:
    player(room &room, session &session, uint32_t player_id, uint32_t index, uint32_t offset)
    : _room(room), _session(session), _player_id(player_id), _index(index), _offset(offset), _max_latency(0)
    {
    }
    
    void toEntity(Entity *entity);
    
    inline uint32_t get_player_id()
    {
        return _player_id;
    }
    
    inline room &get_room()
    {
        return _room;
    }
    
    inline session &get_session()
    {
        return _session;
    }
    
private:
    inline void set_player_id(uint32_t player_id)
    {
        _player_id = player_id;
    }
    
private:
    
    room &_room;
    session &_session;
    uint32_t _player_id;
    uint32_t _index;
    uint32_t _offset;
    int32_t _direction;
    int32_t _max_latency;
    std::queue<uint32_t> _old_routes;
    friend class room;
};
#endif
