#ifndef _PLAYER_H_
#define _PLAYER_H_
#include "common.h"
#include <queue>

class player {
public:
    player(room &room, session &session, uint32_t player_id, uint32_t index, uint32_t offset)
    : _room(room), _session(session), _player_id(player_id), _index(index), _offset(offset), _direction(-1), _max_latency(0), _action_top(0), _action_count(0)
    {
    }
    
    void toEntity(demo::protocol::Entity *entity);
    
    uint32_t get_player_id()
    {
        return _player_id;
    }
    
    room &get_room()
    {
        return _room;
    }
    
    session &get_session()
    {
        return _session;
    }
    
    void action_add(const demo::protocol::ActionReq &req)
    {
        std::cout << "req size is " << req.actions_size() << std::endl;
        for (int i = 0; i < req.actions_size(); i++) {
            const demo::protocol::Action &action = req.actions(i);
            uint32_t idx = action.id() & (ACTION_BUF_SIZE - 1);
            // actions in req come in reverse order
            if (_actions[idx].use_count() && _actions[idx]->id() == action.id()) {
                std::cout << "break size is " << i << std::endl;
                break;
            }
            _actions[idx] = std::make_shared<demo::protocol::Action>();
            _actions[idx]->CopyFrom(action);
            _action_count++;
        }
    }
    
    action_shared_ptr& action_top() {
        uint32_t idx = _action_top & (ACTION_BUF_SIZE - 1);
        return _actions[idx];
    }
    
    uint32_t action_count() {
        return _action_count;
    }
    
    void action_pop() {
        _action_top++;
        _action_count--;
    }
    
private:
    void set_player_id(uint32_t player_id)
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
    // ring buffer
    action_shared_ptr _actions[ACTION_BUF_SIZE];
    uint32_t _action_count;
    // ring buffer front
    uint32_t _action_top;
    friend class room;
};
#endif
