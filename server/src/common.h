#ifndef _COMMON_H_
#define _COMMON_H_
#include <memory>
#include <cstdint>
#include <ctime>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <uv.h>
#include <uWS.h>
#include "messages.pb.h"

typedef enum direction {
    DIRECTION_RIGHT = 0,
    DIRECTION_DOWN  = 1,
    DIRECTION_LEFT  = 2,
    DIRECTION_UP    = 3,
    DIRECTION_NONE  = 4
} direction_t;

#define DIRECTION_VALID(dir) ((dir) >= DIRECTION_RIGHT && (dir) <= DIRECTION_UP)
inline uint32_t opposite_direction(uint32_t direction)
{
    switch (direction) {
    case DIRECTION_UP:
        return DIRECTION_DOWN;
    case DIRECTION_DOWN:
        return DIRECTION_UP;
    case DIRECTION_LEFT:
        return DIRECTION_RIGHT;
    case DIRECTION_RIGHT:
        return DIRECTION_LEFT;
    default:
        return DIRECTION_NONE;
    }
}

#define TILE_TYPE_ROAD 1
#define TILE_TYPE_WALL 0

#define TILE_SHIFT_BITS 10
#define TICK_INTERVAL 50
#define ACTION_BUF_SIZE 64
#define PACKAGE_MTU 1500

typedef uWS::WebSocket<uWS::SERVER> uws_connt_t;
typedef uWS::UpgradeInfo upgrade_info_t;
typedef uWS::OpCode opcode_t;
typedef std::shared_ptr<demo::protocol::Action> action_shared_ptr;
typedef std::shared_ptr<char> package_shared_ptr;
class server;
class session;
class room;
class player;
class conn;

inline uint64_t addr2int(struct sockaddr_in *addr)
{
    uint64_t ret = (addr->sin_addr.s_addr << 16) + addr->sin_port;
    return ret;
}

inline uint64_t get_timestamp_millis()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)(tv.tv_sec) * 1000 + (uint64_t)(tv.tv_usec) / 1000;
}

#endif
