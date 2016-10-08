CC = c++
PROTOC = protoc
CFLAGS = -Idep/tmx/src -Idep/websocketpp -g -std=c++11 -Wall
LDFLAGS = -Ldep/tmx/build -ltmx -lz -lxml2 -lprotobuf -lboost_system
SOURCES = src/tile_map.cc src/main.cc

all: server test

clean:
	rm -rf server test src/*.o src/messages.pb.h src/messages.pb.cc

server: src/main.o src/tile_map.o src/messages.pb.o src/server.o src/room.o src/player.o
	${CC} -o $@ $^ ${LDFLAGS}

test: src/test.o src/tile_map.o src/messages.pb.o src/server.o src/room.o src/session.o src/player.o
	${CC} -o $@ $^ ${LDFLAGS}

src/%.o: src/%.cc src/messages.pb.h
	${CC} -o $@ ${CFLAGS} -c $<

src/messages.pb.cc: proto/messages.proto
	${PROTOC} --proto_path=proto --cpp_out=src $^

src/messages.pb.h: proto/messages.proto
	${PROTOC} --proto_path=proto --cpp_out=src $^
