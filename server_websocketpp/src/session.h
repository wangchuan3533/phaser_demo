#ifndef _SESSION_H_
#define _SESSION_H_
#include "common.h"
class room;
class session {
public:
    session(asio_server_t &svr, connection_hdl hdl)
    : _svr(svr), _hdl(hdl), _room(NULL), _player_id(0)
    {
    }
    
    inline const char *get_nickname()
    {
        return _nickname;
    }
    
    inline void set_nickname(const char *nickname)
    {
        strncpy(_nickname, nickname, sizeof(_nickname));
    }
    
    inline websocketpp::connection_hdl get_hdl()
    {
        return _hdl;
    }
    
    inline room *get_room()
    {
        return _room;
    }
    
    inline uint32_t get_player_id()
    {
        return _player_id;
    }
    
    inline void send(const std::string &str)
    {
        _svr.send(_hdl, str, websocketpp::frame::opcode::binary);
    }
    
private:
    inline void set_room(room *room)
    {
        _room = room;
    }
    
    inline void set_player_id(uint32_t player_id)
    {
        _player_id = player_id;
    }
    
private:
    asio_server_t &_svr;
    websocketpp::connection_hdl _hdl;
    char _nickname[128];
    room *_room;
    uint32_t _player_id;
    friend class room;
};
#endif
