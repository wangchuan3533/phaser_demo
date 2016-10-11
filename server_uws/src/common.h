#ifndef _COMMON_H_
#define _COMMON_H_
#include <cstdint>
#include <ctime>
#include <cstdlib>
#include <iostream>

#include <uWS.h>
#include "messages.pb.h"

#define DIRECTION_RIGHT 0
#define DIRECTION_DOWN  1
#define DIRECTION_LEFT  2
#define DIRECTION_UP    3
#define DIRECTION_VALID(dir) ((dir) >= 0 && (dir) <= 3)
inline int32_t opposite_direction(int32_t direction)
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
        return 4;
    }
}

#define TILE_TYPE_ROAD 1
#define TILE_TYPE_WALL 0

#define TILE_SHIFT_BITS 10
#define TICK_INTERVAL 50

typedef uWS::WebSocket<uWS::SERVER> connection_t;
typedef uWS::UpgradeInfo upgrade_info_t;
typedef uWS::OpCode opcode_t;
typedef std::shared_ptr<ActionReq> action_req_ptr;
typedef std::shared_ptr<ActionRes> action_res_ptr;

class server;
class session;
class room;
class player;

inline uint64_t get_timestamp_millis()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (uint64_t)(tv.tv_sec) * 1000 + (uint64_t)(tv.tv_usec) / 1000;
}

#endif