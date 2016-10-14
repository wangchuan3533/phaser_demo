#ifndef _SERVER_H_
#define _SERVER_H_
#include <map>
#include "common.h"
#include "session.h"

class server {
public:
    
    server();
    virtual ~server();
    
    void on_open(uws_connt_t conn, upgrade_info_t ui);
    void on_close(uws_connt_t conn, int code, char *message, size_t length);
    void on_message(uws_connt_t conn, char *message, size_t length, opcode_t opcode);
    static void on_timer(uv_timer_t *timer);
    void broadcast(char *data, size_t len);
    void run();
    
    static void udp_on_recv(uv_udp_t* handle, ssize_t nread, const uv_buf_t *buf, const struct sockaddr* addr, unsigned flags);
    static void udp_on_alloc(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf);
    void handle_message(session *s, char *message, size_t length);
    
private:
    std::map<uws_connt_t, session *> _uws_sessions;
    std::map<uint64_t, session *> _udp_sessions;
    uWS::Hub _hub;
};
#endif
