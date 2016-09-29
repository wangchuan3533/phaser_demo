CC = c++
CFLAGS = -I../dep/include -g
LDFLAGS = -L../dep/lib -lz -lxml2 -ltmx -luv -lwebsockets -lssl -lcrypto -lprotobuf
SOURCES = tile_map.cc

all: server test

clean:
	rm -rf server test *.o

server: main.o tile_map.o messages.pb.o
	${CC} -o $@ ${LDFLAGS} $^

test: test.o tile_map.o messages.pb.o
	${CC} -o $@ ${LDFLAGS} $^

main.o: main.cc
	${CC} -o $@ ${CFLAGS} -c $^
	
test.o: test.cc
	${CC} -o $@ ${CFLAGS} -c $^

tile_map.o: tile_map.cc
	${CC} -o $@ ${CFLAGS} -c $^
	
messages.pb.o: messages.pb.cc
	${CC} -o $@ ${CFLAGS} -c $^
