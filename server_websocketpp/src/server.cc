#include "common.h"
#include "server.h"
#include "room.h"
#include "session.h"
#include "player.h"


server::server() : _room_id_seed(0)
{
    // Set logging settings
    _svr.set_access_channels(websocketpp::log::alevel::none);
    //_svr.clear_access_channels(websocketpp::log::alevel::frame_payload);
    
    // init asio
    _svr.init_asio();
    
    // register handlers
    _svr.set_open_handler(bind(&server::on_open, this, ::_1));
    _svr.set_close_handler(bind(&server::on_close, this, ::_1));
    _svr.set_message_handler(bind(&server::on_message, this, ::_1, ::_2));
    _svr.set_socket_init_handler(bind(&server::on_socket_init, this, ::_1, ::_2));
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

void server::on_open(connection_hdl hdl)
{
    connection_ptr con = _svr.get_con_from_hdl(hdl);
    session *s = new session(_svr, hdl);
    s->set_nickname(con->get_uri()->get_resource().c_str());
    _sessions[hdl] = s;
    std::cout << s->get_nickname() << " joined" << std::endl;
}

void server::on_close(connection_hdl hdl)
{
    auto it = _sessions.find(hdl);
    if (it == _sessions.end()) {
        std::cerr << "on_close no session found" << std::endl;
        return;
    }
    
    session *s = it->second;
    room *r = s->get_room();
    if (r) {
        r->player_leave(s);
    }
    
    _sessions.erase(hdl);
    delete s;
}

void server::on_socket_init(connection_hdl hdl, boost::asio::ip::tcp::socket &s)
{
    boost::asio::ip::tcp::no_delay option(true);
    s.set_option(option);
}

void server::on_message(connection_hdl hdl, message_ptr msg)
{
    auto it = _sessions.find(hdl);
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
    
    message_req.ParseFromString(msg->get_payload());
    
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
        _svr.send(hdl, message_res.SerializeAsString(), websocketpp::frame::opcode::binary);
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
        _svr.send(hdl, message_res.SerializeAsString(), websocketpp::frame::opcode::binary);
        
        break;
    case TIME_SYNC_REQ:
        time_sync_req.ParseFromString(message_req.data());
        time_sync_res.set_client_send_time(time_sync_req.client_send_time());
        time_sync_res.set_server_recv_time(get_timestamp_millis());
        time_sync_res.set_server_send_time(get_timestamp_millis());
        message_res.set_type(TIME_SYNC_RES);
        message_res.set_data(time_sync_res.SerializeAsString());
        _svr.send(hdl, message_res.SerializeAsString(), websocketpp::frame::opcode::binary);
        break;
    default:
        break;
    }
    
}

void server::on_timer(const websocketpp::lib::error_code &ec)
{
    
    for (auto & it : _rooms) {
        it.second->update();
    }
    _svr.set_timer(TICK_INTERVAL, bind(&server::on_timer, this, ::_1));
}

void server::broadcast(void *data, size_t len)
{
    for (auto & it : _sessions) {
        _svr.send(it.second->get_hdl(), data, len, websocketpp::frame::opcode::text);
    }
}

void server::run()
{
    // listen
    _svr.listen(9002);
    
    // start accept
    _svr.start_accept();
    
    // start timer
    _svr.set_timer(TICK_INTERVAL, bind(&server::on_timer, this, ::_1));
    
    // run
    try {
        _svr.run();
    } catch (const websocketpp::exception &e) {
        std::cout << e.what() << std::endl;
    } catch (const std::exception &e) {
        std::cout << e.what() << std::endl;
        _svr.stop();
    }
}
