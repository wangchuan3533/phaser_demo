CC = c++
CFLAGS = -Idep/include -g -std=c++11 -Wall
LDFLAGS = -Ldep/lib -lz -lxml2 -ltmx -lprotobuf -lboost_system
SOURCES = src/tile_map.cc src/main.cc

all: server test

clean:
	rm -rf server test src/*.o

server: src/main.o src/tile_map.o src/messages.pb.o src/server.o src/room.o src/player.o
	${CC} -o $@ ${LDFLAGS} $^

test: src/test.o src/tile_map.o src/messages.pb.o src/server.o src/room.o src/session.o src/player.o
	${CC} -o $@ ${LDFLAGS} $^

src/%.o : src/%.cc
	${CC} -o $@ ${CFLAGS} -c $^
