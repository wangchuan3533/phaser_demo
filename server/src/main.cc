#include <iostream>
#include <map>
#include "common.h"
#include "room.h"
#include "server.h"

int main()
{
    room::create_room();
    server svr;
    svr.run();
}
