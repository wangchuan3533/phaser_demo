#include <time.h>
#include "common.h"
#include "room.h"
#include "player.h"
#include "session.h"
static const uint32_t speed = 6 << TILE_SHIFT_BITS;
uint32_t room::_room_id_seed = 0;
std::map<uint32_t, room*> room::_rooms;

#define TILE_MAP_DATA_FILE "data/8.data"
#define TILE_MAP_DATA_FILE_PB "data/8.pb.data"

room::room(uint32_t room_id)
: _room_id(room_id), _player_id_seed(0), _elapsed(0)
{
    // TODO singleton tile_map
    _tile_map = new tile_map;
    _tile_map->load_from_pb(TILE_MAP_DATA_FILE_PB);
    //_tile_map->load(TILE_MAP_DATA_FILE);
    
    _start_time = get_timestamp_millis();
}

room::~room()
{
    delete _tile_map;
}

bool room::player_join(session *s, const demo::protocol::JoinRoomReq &join_room_req, demo::protocol::JoinRoomRes &join_room_res)
{
    uint32_t player_id = _player_id_seed++;
    position_t pos = _tile_map->random_position();
    _players[player_id] = new player(*this, *s, player_id, pos.index, pos.offset);
    for (auto & it : _players) {
        demo::protocol::Entity *entity = join_room_res.add_entities();
        it.second->toEntity(entity);
    }
    s->set_room(this);
    s->set_player_id(player_id);
    
    join_room_res.set_ret(0);
    join_room_res.set_room_id(_room_id);
    join_room_res.set_player_id(player_id);
    join_room_res.set_elapsed(_elapsed);
    join_room_res.set_start_time(_start_time);
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

bool room::player_action(uint32_t player_id, const demo::protocol::ActionReq &action_req, demo::protocol::ActionRes &action_res)
{
    uint64_t now = get_timestamp_millis();
    uint32_t now_elapsed = now - _start_time;
    player *p = _players[player_id];
    int32_t latency = ((int32_t)now_elapsed) - ((int32_t)action_req.elapsed());
    if (latency > p->_max_latency) p->_max_latency = latency;
    p->action_add(action_req);
    std::cout << "[latency] " << latency << std::endl;
    std::cout << "[max_latency] " << p->_max_latency << std::endl;
    
    action_res.set_ret(0);
    action_res.set_id(action_req.actions(0).id());
    return true;
}

bool room::update()
{
    uint64_t now = get_timestamp_millis();
    uint32_t now_elapsed = now - _start_time;
    uint32_t delta_time = now_elapsed - _elapsed;
    _elapsed = now_elapsed;
    
    demo::protocol::Message msg;
    demo::protocol::UpdateNtf *ntf = msg.mutable_update_ntf();
    
    ntf->set_elapsed(_elapsed);
    
    //std::cout << "room tick" << std::endl;
    for (auto & it : _players) {
        uint32_t player_id = it.first;
        player *p = _players[player_id];
        action_shared_ptr &action = p->action_top();
        if (action.use_count() > 0) {
            uint32_t gap = ((int32_t)_elapsed) - ((int32_t)action->elapsed());
            if (gap < p->_max_latency + 20) goto _update_pos;
            
            if (action->index() == p->_index) {
                p->_direction = action->direction();
            } else if (!p->_old_routes.empty() && action->index() == p->_old_routes.back()){
                // TODO check
                edge_t *old_edge = _tile_map->get_edge(action->index());
                p->_index = action->index();
                p->_offset = old_edge->length + p->_offset;
                p->_direction = action->direction();
            } else {
                std::cout << "action:" << action->index() << "," << action->offset() << "," << action->direction() << "," << std::endl;
                std::cout << "player:" << p->_index << "," << p->_offset << "," << p->_direction << "," << std::endl;
                goto _update_pos;
                //if (gap < p->_max_latency + 100) continue;
            }
            p->action_pop();
        }
_update_pos:
        
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
        
        demo::protocol::Entity *entity = ntf->add_entities();
        p->toEntity(entity);
    }
    
    msg.set_type(demo::protocol::UPDATE_NTF);
    
    size_t package_len = msg.ByteSize();
    if (package_len > PACKAGE_MTU) {
        std::cerr << "package large than mtu " << package_len << std::endl;
        return false;
    }
    
    package_shared_ptr package((char *)malloc(PACKAGE_MTU), free);
    if (!msg.SerializeToArray(package.get(), PACKAGE_MTU)) {
        std::cerr << "package serialize failed " << std::endl;
        return false;
    }
    
    for (auto & it : _players) {
        it.second->get_session().send(package, package_len);
    }
    
    return true;
}
