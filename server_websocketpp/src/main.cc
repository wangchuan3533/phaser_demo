#include <iostream>
#include <map>
#include "common.h"
#include "server.h"

int main()
{
    server svr;
    svr.create_room();
    svr.run();
}
