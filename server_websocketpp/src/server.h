#ifndef _SERVER_H_
#define _SERVER_H_
#include <map>
#include "common.h"

using websocketpp::lib::bind;
using websocketpp::lib::placeholders::_1;
using websocketpp::lib::placeholders::_2;

class server {
public:
    
    server();
    virtual ~server();
    
    void on_open(connection_hdl hdl);
    void on_close(connection_hdl hdl);
    void on_socket_init(connection_hdl hdl, boost::asio::ip::tcp::socket &);
    void on_message(connection_hdl hdl, message_ptr msg);
    void on_timer(const websocketpp::lib::error_code &ec);
    void broadcast(void *data, size_t len);
    void run();
    
    
    room *create_room();
    room *get_room(uint32_t room_id);
    
private:
    uint32_t _room_id_seed;
    std::map<connection_hdl, session *, std::owner_less<connection_hdl>> _sessions;
    std::map<uint32_t, room *> _rooms;
    asio_server_t _svr;
    
};
#endif
