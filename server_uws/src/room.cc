#include <time.h>
#include "common.h"
#include "room.h"
#include "player.h"
#include "session.h"
static const uint32_t speed = 6 << TILE_SHIFT_BITS;


#define TILE_MAP_DATA_FILE "data/8.data"
room::room(uint32_t room_id)
: _room_id(room_id), _player_id_seed(0), _elapsed(0)
{
    // TODO singleton tile_map
    _tile_map = new tile_map;
    _tile_map->load(TILE_MAP_DATA_FILE);
    
    _start_time = get_timestamp_millis();
}

room::~room()
{
    delete _tile_map;
}

bool room::player_join(session *s, JoinRoomRes *join_room_res)
{
    uint32_t player_id = _player_id_seed++;
    position_t pos = _tile_map->random_position();
    _players[player_id] = new player(*this, *s, player_id, pos.index, pos.offset);
    for (auto & it : _players) {
        Entity *entity = join_room_res->add_entities();
        it.second->toEntity(entity);
    }
    s->set_room(this);
    s->set_player_id(player_id);
    
    join_room_res->set_ret(0);
    join_room_res->set_room_id(_room_id);
    join_room_res->set_player_id(player_id);
    join_room_res->set_elapsed(_elapsed);
    join_room_res->set_start_time(_start_time);
    return true;
}

bool room::player_leave(session *s)
{
    uint32_t player_id = s->get_player_id();
    s->set_room(NULL);
    s->set_player_id(0);
    delete _players[player_id];
    _players.erase(player_id);
    return true;
}

bool room::player_action(uint32_t player_id, action_req_shared_ptr action_req, ActionRes *action_res)
{
    uint64_t now = get_timestamp_millis();
    uint32_t now_elapsed = now - _start_time;
    player *p = _players[player_id];
    int32_t latency = ((int32_t)now_elapsed) - ((int32_t)action_req->elapsed());
    if (latency > p->_max_latency) p->_max_latency = latency;
    _actions_queues[player_id].push_back(action_req);
    std::cout << "[latency] " << latency << std::endl;
    std::cout << "[max_latency] " << p->_max_latency << std::endl;
    
    action_res->set_ret(0);
    action_res->set_id(0);
    action_res->set_elapsed(_elapsed);
    return true;
}

bool room::update()
{
    uint64_t now = get_timestamp_millis();
    uint32_t now_elapsed = now - _start_time;
    uint32_t delta_time = now_elapsed - _elapsed;
    _elapsed = now_elapsed;
    
    Message msg;
    UpdateNtf *ntf = msg.mutable_update_ntf();
    ntf->set_elapsed(_elapsed);
    
    //std::cout << "room tick" << std::endl;
    for (auto & it : _players) {
        uint32_t player_id = it.first;
        player *p = _players[player_id];
        action_queue &q = _actions_queues[player_id];
        while (!q.empty()) {
            action_req_shared_ptr action_req = q.front();
            if (((int32_t)_elapsed) - ((int32_t)action_req->elapsed()) < p->_max_latency + 20) break;
            
            if (action_req->index() == p->_index) {
                p->_direction = action_req->direction();
            } else if (!p->_old_routes.empty() && action_req->index() == p->_old_routes.back()){
                // TODO check
                edge_t *old_edge = _tile_map->get_edge(action_req->index());
                p->_index = action_req->index();
                p->_offset = old_edge->length + p->_offset;
                p->_direction = action_req->direction();
            } else {
                std::cout << "action:" << action_req->index() << "," << action_req->offset() << "," << action_req->direction() << "," << std::endl;
                std::cout << "player:" << p->_index << "," << p->_offset << "," << p->_direction << "," << std::endl;
                break;
            }
            q.pop_front();
        }
        
        edge_t *edge = _tile_map->get_edge(p->_index);
        uint32_t delta_offset = speed * delta_time / 1000;
        if (p->_offset < edge->length) {
            if (p->_direction == opposite_direction(edge->direction)) {
                p->_old_routes.push(p->_index);
                p->_index ^= 1;
                p->_offset = edge->length - p->_offset;
                p->_direction = -1;
            }
            p->_offset += delta_offset;
        } else {
            vertex_t *vertex = _tile_map->get_vertex(edge->dst);
            if (DIRECTION_VALID(p->_direction) && vertex->idx_out_edges[p->_direction] >= 0) {
                p->_old_routes.push(p->_index);
                p->_index = vertex->idx_out_edges[p->_direction];
                p->_offset = p->_offset - edge->length + delta_offset;
                p->_direction = -1;
            } else if (vertex->idx_out_edges[edge->direction] >= 0) {
                p->_old_routes.push(p->_index);
                p->_index = vertex->idx_out_edges[edge->direction];
                p->_offset = p->_offset - edge->length + delta_offset;
            }
        }
        
        Entity *entity = ntf->add_entities();
        p->toEntity(entity);
    }
    
    msg.set_type(UPDATE_NTF);
    std::string s;
    msg.SerializeToString(&s);
    
    for (auto & it : _players) {
        it.second->get_session().send(s);
    }
    
    return true;
}
