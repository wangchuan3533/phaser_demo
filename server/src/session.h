#ifndef _SESSION_H_
#define _SESSION_H_
#include "common.h"
#include "conn.h"
class session {
public:
    session(conn *conn)
    : _conn(conn), _room(NULL), _player_id(0)
    {
    }
    
    inline const char *get_nickname()
    {
        return _nickname;
    }
    
    inline void set_nickname(const char *str, size_t len)
    {
        if (len > sizeof(_nickname)) len = sizeof(_nickname) - 1;
        memcpy(_nickname, str, len);
        _nickname[len] = '\0';
    }
    
    inline room *get_room()
    {
        return _room;
    }
    
    inline uint32_t get_player_id()
    {
        return _player_id;
    }
    
    inline void send(package_shared_ptr &package, size_t length)
    {
        _conn->send(package, length);
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
    conn *_conn;
    char _nickname[128];
    room *_room;
    uint32_t _player_id;
    friend class room;
};
#endif
