#ifndef _SERVER_H_
#define _SERVER_H_
#include <map>
#include "common.h"

class server {
public:
    
    server();
    virtual ~server();
    
    void on_open(connection_t conn, upgrade_info_t ui);
    void on_close(connection_t conn, int code, char *message, size_t length);
    void on_message(connection_t conn, char *message, size_t length, opcode_t opcode);
    static void on_timer(uv_timer_t *timer);
    void broadcast(char *data, size_t len);
    void run();
    
    room *create_room();
    room *get_room(uint32_t room_id);
    
private:
    uint32_t _room_id_seed;
    std::map<connection_t, session *> _sessions;
    std::map<uint32_t, room *> _rooms;
    uWS::Hub _hub;
};
#endif
