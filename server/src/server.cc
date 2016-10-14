#include "common.h"
#include "server.h"
#include "room.h"
#include "conn.h"
#include "session.h"
#include "player.h"

server::server()
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

void server::on_open(uws_connt_t conn, upgrade_info_t ui)
{
    conn_ws *cws = new conn_ws(conn);
    session *s = new session(cws);
    s->set_nickname(ui.path + 1, ui.pathLength - 1);
    _uws_sessions[conn] = s;
    std::cout << "sizeof uws_connt_t is " << sizeof(uws_connt_t) << std::endl;
    std::cout << "websocket player " << s->get_nickname() << " joined" << std::endl;
}

void server::on_close(uws_connt_t conn, int code, char *message, size_t len)
{
    auto it = _uws_sessions.find(conn);
    if (it == _uws_sessions.end()) {
        std::cerr << "on_close no session found" << std::endl;
        return;
    }
    
    session *s = it->second;
    room *r = s->get_room();
    if (r) {
        r->player_leave(s);
    }
    
    _uws_sessions.erase(conn);
    delete s;
}

void server::on_message(uws_connt_t conn, char *msg, size_t len, opcode_t opcode)
{
    auto it = _uws_sessions.find(conn);
    if (it == _uws_sessions.end()) {
        std::cerr << "on_message no session found" << std::endl;
        return;
    }
    
    handle_message(it->second, msg, len);
}

void server::on_timer(uv_timer_t *timer)
{
    //server *svr = (server *)timer->data;
    room::update_all();
}

void server::broadcast(char *data, size_t len)
{
    for (auto & it : _uws_sessions) {
        uws_connt_t conn = it.first;
        conn.send(data, len, opcode_t::TEXT);
    }
}

void server::udp_on_recv(uv_udp_t *req, ssize_t nread, const uv_buf_t *buf, const struct sockaddr* addr, unsigned flags)
{
    if (nread < 0) {
        std::cerr << "udp read error :" << uv_err_name(nread) << std::endl;
        uv_close((uv_handle_t *)req, NULL);
        free(buf->base);
        return;
    }
    if (nread == 0 || addr == NULL) return;
    server *svr = (server *)req->data;
    uint64_t int_addr = addr2int((struct sockaddr_in *)addr);
    std::cout << "int_addr" << int_addr << std::endl;
    std::cout << "nread:" << nread << std::endl;
    //std::cout << "length:" << buf->len << std::endl;
    auto it = svr->_udp_sessions.find(int_addr);
    
    if (it == svr->_udp_sessions.end()) {
        if (strncmp("hello", (const char *)buf->base, 5)) return;
        char name[16];
        uv_ip4_name((const struct sockaddr_in *)addr, name, sizeof(name));
        std::cout << "remote address is " << name << std::endl;
        conn_udp *cudp = new conn_udp(addr, svr->_hub.getLoop());
        session *s = new session(cudp);
        s->set_nickname(buf->base, nread);
        svr->_udp_sessions[int_addr] = s;
        std::cout << "new udp player " << s->get_nickname() << " joined" << std::endl;
        return;
    }
    
    svr->handle_message(it->second, buf->base, nread);
    free(buf->base);
}

void server::udp_on_alloc(uv_handle_t* req, size_t suggested_size, uv_buf_t* buf)
{
    buf->base = (char *)malloc(suggested_size);
    buf->len = suggested_size;
}

void server::handle_message(session *s, char *message, size_t length)
{
    demo::protocol::Message message_req;
    demo::protocol::Message message_res;
    demo::protocol::JoinRoomRes *join_room_res;
    demo::protocol::ActionRes *action_res;
    demo::protocol::TimeSyncRes *time_sync_res;
    action_req_shared_ptr action_req;
    
    uint32_t room_id;
    room *r;
    
    message_req.ParseFromArray(message, length);
    //std::cout << "message type:" << message_req.type() << std::endl;
    std::string tmp;
    
    switch (message_req.type()) {
    case demo::protocol::JOIN_ROOM_REQ:
        room_id = message_req.join_room_req().room_id();
        join_room_res = message_res.mutable_join_room_res();
        std::cout << "join room room_id " << room_id << std::endl;
        r = room::get_room(room_id);
        if (r) {
            if (!(r->player_join(s, join_room_res))) {
                std::cerr << "join room failed" << std::endl;
                join_room_res->set_ret(1);
            }
        } else {
            join_room_res->set_ret(1);
        }
        
        message_res.set_type(demo::protocol::JOIN_ROOM_RES);
        message_res.SerializeToString(&tmp);
        s->send((void *)tmp.c_str(), tmp.size());
        break;
    case demo::protocol::ACTION_REQ:
        
        if (!(r = s->get_room())) {
            std::cerr << "action not in room" << std::endl;
            break;
        }
        
        action_req = std::make_shared<demo::protocol::ActionReq>();
        action_req->CopyFrom(message_req.action_req());
        action_res = message_res.mutable_action_res();
        r->player_action(s->get_player_id(), action_req, action_res);
        
        message_res.set_type(demo::protocol::ACTION_RES);
        message_res.SerializeToString(&tmp);
        s->send((void *)tmp.c_str(), tmp.size());
        
        break;
    case demo::protocol::TIME_SYNC_REQ:
        time_sync_res = message_res.mutable_time_sync_res();
        time_sync_res->set_client_send_time(message_req.time_sync_req().client_send_time());
        time_sync_res->set_server_recv_time(get_timestamp_millis());
        time_sync_res->set_server_send_time(get_timestamp_millis());
        message_res.set_type(demo::protocol::TIME_SYNC_RES);
        message_res.SerializeToString(&tmp);
        s->send((void *)tmp.c_str(), tmp.size());
        break;
    default:
        break;
    }
}

void server::run()
{
    uv_loop_t *uv_loop = _hub.getLoop();
    
    // start udp server
    uv_udp_t udp_server;
    struct sockaddr_in addr;
    uv_udp_init(uv_loop, &udp_server);
    udp_server.data = this;
    uv_ip4_addr("0.0.0.0", 11111, &addr);
    uv_udp_bind(&udp_server, (const struct sockaddr *)&addr, UV_UDP_REUSEADDR);
    uv_udp_recv_start(&udp_server, &udp_on_alloc, &udp_on_recv);
    
    // start timer
    uv_timer_t timer;
    uv_timer_init(uv_loop, &timer);
    timer.data = this;
    uv_timer_start(&timer, on_timer, TICK_INTERVAL, TICK_INTERVAL);
    
    // start websocket
    _hub.listen(9002);
    
    _hub.run();
}
