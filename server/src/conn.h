#ifndef _CONN_H_
#define _CONN_H_
#include "common.h"

class conn {
public:
    virtual void send(package_shared_ptr package, size_t length) = 0;
};

class conn_ws: public conn {
public:
    conn_ws(uws_connt_t uws_conn): _uws_conn(uws_conn)
    {
        
    }
    
    void send(package_shared_ptr package, size_t length)
    {
        _uws_conn.send(package.get(), length, opcode_t::BINARY);
    }
private:
    uws_connt_t _uws_conn;
};

class conn_udp: public conn {
public:
    
    conn_udp(const struct sockaddr *addr, uv_udp_t *uv_udp) : _uv_udp(uv_udp), _addr(*addr)
    {
    }
    
    void send(package_shared_ptr package, size_t length)
    {
        package_shared_ptr *ptr = new package_shared_ptr(package);
        uv_buf_t buf = uv_buf_init(package.get(), length);
        uv_udp_send_t *req = (uv_udp_send_t *)malloc(sizeof(uv_udp_send_t));
        req->data = ptr;
        uv_udp_send(req, _uv_udp, &buf, 1, &_addr, udp_on_send);
    }
    
    static void udp_on_send(uv_udp_send_t *req, int status)
    {
        if (status != 0) {
            std::cerr << "udp send cb error " << uv_err_name(status) << std::endl;
        }
        delete ((package_shared_ptr *)req->data);
        free(req);
    }
    
private:
    uv_udp_t *_uv_udp;
    struct sockaddr _addr;
};
#endif
