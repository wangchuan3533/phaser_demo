#include "common.h"
#include "server.h"
#include "room.h"
#include "session.h"
#include "player.h"


server::server() : _room_id_seed(0)
{
    using namespace std::placeholders;
    
    // register handlers
    
    _hub.onConnection(std::bind(&server::on_open, this, _1, _2));
    _hub.onMessage(std::bind(&server::on_message, this, _1, _2, _3, _4));
    _hub.onDisconnection(std::bind(&server::on_close, this, _1, _2, _3, _4));
    
}

server::~server()
{
}

room *server::create_room()
{
    room *r = new room(_room_id_seed);
    _rooms[_room_id_seed++] = r;
    return r;
}

inline room *server::get_room(uint32_t room_id)
{
    auto it = _rooms.find(room_id);
    if (it == _rooms.end()) return NULL;
    return it->second;
}

void server::on_open(connection_t conn, upgrade_info_t ui)
{
    session *s = new session( conn);
    s->set_nickname(ui.path + 1, ui.pathLength - 1);
    _sessions[conn] = s;
    std::cout << "sizeof connection_t is " << sizeof(connection_t) << std::endl;
    std::cout << s->get_nickname() << " joined" << std::endl;
}

void server::on_close(connection_t conn, int code, char *message, size_t len)
{
    auto it = _sessions.find(conn);
    if (it == _sessions.end()) {
        std::cerr << "on_close no session found" << std::endl;
        return;
    }
    
    session *s = it->second;
    room *r = s->get_room();
    if (r) {
        r->player_leave(s);
    }
    
    _sessions.erase(conn);
    delete s;
}

void server::on_message(connection_t conn, char *msg, size_t len, opcode_t opcode)
{
    auto it = _sessions.find(conn);
    if (it == _sessions.end()) {
        std::cerr << "on_message no session found" << std::endl;
        return;
    }
    
    session *s = it->second;
    
    Message message_req;
    Message message_res;
    
    JoinRoomReq join_room_req;
    JoinRoomRes join_room_res;
    uint32_t room_id;
    room *r;
    
    action_req_ptr action_req;
    action_res_ptr action_res;
    
    TimeSyncReq time_sync_req;
    TimeSyncRes time_sync_res;
    
    message_req.ParseFromArray(msg, len);
    std::string tmp;
    
    switch (message_req.type()) {
    case JOIN_ROOM_REQ:
        join_room_req.ParseFromString(message_req.data());
        room_id = join_room_req.room_id();
        std::cout << "join room room_id " << room_id << std::endl;
        r = get_room(room_id);
        if (r) {
            if (!(r->player_join(it->second, join_room_res))) {
                std::cerr << "join room failed" << std::endl;
                join_room_res.set_ret(1);
            }
        } else {
            join_room_res.set_ret(1);
        }
        
        message_res.set_type(JOIN_ROOM_RES);
        message_res.set_data(join_room_res.SerializeAsString());
        message_res.SerializeToString(&tmp);
        conn.send(tmp.c_str(), tmp.size(), opcode_t::BINARY);
        break;
    case ACTION_REQ:
        
        if (!(r = s->get_room())) {
            std::cerr << "action not in room" << std::endl;
            break;
        }
        
        action_req = std::make_shared<ActionReq>();
        action_req->ParseFromString(message_req.data());
        action_res = std::make_shared<ActionRes>();
        r->player_action(s->get_player_id(), action_req, action_res);
        
        message_res.set_type(ACTION_RES);
        message_res.set_data(action_res->SerializeAsString());
        message_res.SerializeToString(&tmp);
        conn.send(tmp.c_str(), tmp.size(), opcode_t::BINARY);
        
        break;
    case TIME_SYNC_REQ:
        time_sync_req.ParseFromString(message_req.data());
        time_sync_res.set_client_send_time(time_sync_req.client_send_time());
        time_sync_res.set_server_recv_time(get_timestamp_millis());
        time_sync_res.set_server_send_time(get_timestamp_millis());
        message_res.set_type(TIME_SYNC_RES);
        message_res.set_data(time_sync_res.SerializeAsString());
        message_res.SerializeToString(&tmp);
        conn.send(tmp.c_str(), tmp.size(), opcode_t::BINARY);
        break;
    default:
        break;
    }
    
}

void server::on_timer(uv_timer_t *timer)
{
    server *_svr = (server *)timer->data;
    for (auto & it : _svr->_rooms) {
        it.second->update();
    }
}

void server::broadcast(char *data, size_t len)
{
    for (auto & it : _sessions) {
        connection_t *conn = (connection_t *)&(it.first);
        conn->send(data, len, opcode_t::TEXT);
    }
}

void server::run()
{
    // listen
    _hub.listen(9002);
    
    uv_timer_t timer;
    uv_timer_init(_hub.getLoop(), &timer);
    timer.data = this;
    uv_timer_start(&timer, on_timer, TICK_INTERVAL, TICK_INTERVAL);
    
    _hub.run();
}
