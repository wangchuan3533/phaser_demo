#include <uWS.h>
#include <map>
#include <cstdio>
#include <cstring>
std::map<const char *, uWS::WebSocket<uWS::SERVER>> connections;
void broadcast(char *message, size_t length)
{
    for (auto &it : connections) {
        it.second.send(message, length, uWS::OpCode::TEXT);
    }
}

void onMessage(uWS::WebSocket<uWS::SERVER> ws, char *message, size_t length, uWS::OpCode opCode)
{
    char buffer[128];
    const char *name = (const char *)ws.getUserData();
    size_t offset = snprintf(buffer, sizeof(buffer), "%s : ", name);
    memcpy(buffer + offset, message, length);
    broadcast(buffer, offset + length);
}

void onConnection(uWS::WebSocket<uWS::SERVER> ws, uWS::UpgradeInfo ui)
{
    char buffer[128];
    char *name = new char[ui.pathLength + 1];
    memcpy(name, ui.path, ui.pathLength);
    name[ui.pathLength] = '\0';
    ws.setUserData(name);
    connections[name] = ws;
    size_t length = snprintf(buffer, sizeof(buffer), "%s joined", name);
    broadcast(buffer, length);
}

void timer_cb(uv_timer_t *timer, int status)
{
    broadcast((char *)"hello", 5);
}

int main()
{
    uWS::Hub h;
    
    h.onConnection(onConnection);
    h.onMessage(onMessage);
    h.listen(3000);
    
    uv_timer_t timer;
    uv_timer_init(h.getLoop(), &timer);
    uv_timer_start(&timer, (uv_timer_cb)&timer_cb, 1000, 1000);
    
    h.run();
}
