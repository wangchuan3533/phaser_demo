CC = cc
CFLAGS = -I../dep/include -g
LDFLAGS = -L../dep/lib -lz -lxml2 -ltmx -luv
SOURCES = tile_map.c

all: server test

clean:
	rm -rf server test *.o

server: main.o tile_map.o
	${CC} -o $@ ${LDFLAGS} $^

test: test.o tile_map.o
	${CC} -o $@ ${LDFLAGS} $^

main.o: main.c
	${CC} -o $@ ${CFLAGS} -c $^
	
test.o: test.c
	${CC} -o $@ ${CFLAGS} -c $^

tile_map.o: tile_map.c
	${CC} -o $@ ${CFLAGS} -c $^
	
