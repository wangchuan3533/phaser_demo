#ifndef _CONN_H_
#define _CONN_H_
#include "common.h"

class conn {
public:
    virtual void send(void *data, size_t length) = 0;
};

class conn_ws: public conn {
public:
    conn_ws(uws_connt_t uws_conn): _uws_conn(uws_conn)
    {
        
    }
    
    void send(void *data, size_t length)
    {
        _uws_conn.send((const char *)data, length, opcode_t::BINARY);
    }
private:
    uws_connt_t _uws_conn;
};

class conn_udp: public conn {
public:
    
    conn_udp(struct sockaddr *addr, uv_loop_t *loop) : _addr(*addr)
    {
        uv_udp_init(loop, &_uv_udp_sock);
    }
    
    void send(void *data, size_t length)
    {
        uv_buf_t buf = uv_buf_init((char *)data, length);
        uv_udp_send_t *req = (uv_udp_send_t *)malloc(sizeof(uv_udp_send_t));
        uv_udp_send(req, &_uv_udp_sock, &buf, 1, &_addr, udp_on_send);
    }
    
    static void udp_on_send(uv_udp_send_t *req, int status)
    {
        if (status != 0) {
            std::cerr << "udp send cb error " << uv_err_name(status) << std::endl;
        }
        free(req);
    }
    
private:
    uv_udp_t _uv_udp_sock;
    struct sockaddr _addr;
};
#endif
