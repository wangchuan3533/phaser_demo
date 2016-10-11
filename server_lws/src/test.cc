#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "tile_map.h"
#include "messages.pb.h"
#define TEST_TMX_FILE_NAME "../data/8.tmx"
#define TEST_TILE_MAP_FILE_NAME "../data/8.data"
int test_tile_map()
{
    struct tile_map tm;
    int ret, i, j, offset;
    
    ret = parse_tile_map(TEST_TMX_FILE_NAME, &tm);
    if (ret != SUCCESS) {
        fprintf(stderr, "parse_tile_map failed\n");
        exit(1);
    }
    
    // save
    ret = save_tile_map(TEST_TILE_MAP_FILE_NAME, &tm);
    if (ret != SUCCESS) {
        fprintf(stderr, "save_tile_map failed\n");
        exit(1);
    }
    
    // free
    release_tile_map(&tm);
    
    // load
    ret = load_tile_map(TEST_TILE_MAP_FILE_NAME, &tm);
    if (ret != SUCCESS) {
        fprintf(stderr, "save_tile_map failed\n");
        exit(1);
    }
    
    for (i = 0; i < tm.n_vertices; i++) {
        printf("[vertex] index: %d, position: [%d, %d], edges: [%d, %d, %d, %d]\n",
            i, tm.vertices[i].x, tm.vertices[i].y,
            tm.vertices[i].idx_out_edges[0],
            tm.vertices[i].idx_out_edges[1],
            tm.vertices[i].idx_out_edges[2],
            tm.vertices[i].idx_out_edges[3]
        );
    }
    
    for (i = 0; i < tm.n_edges; i++) {
        printf("[edge] index: %d, src: [%d, %d], dst: [%d, %d], length: %d, direction: %d\n",
            i, tm.vertices[tm.edges[i].src].x, tm.vertices[tm.edges[i].src].y,
            tm.vertices[tm.edges[i].dst].x, tm.vertices[tm.edges[i].dst].y,
            tm.edges[i].length, tm.edges[i].direction);
    }
    
    for (i = 0; i < tm.n_vertices; i++) {
        for (j = 0; j < tm.n_vertices; j++) {
            offset = i * tm.n_vertices + j;
            printf("[routing] from: %d, to: %d, distance: %d, next: %d\n",
                i, j, tm.route_table[offset].distance, tm.route_table[offset].next);
        }
    }
    
    ret = find_route(0, 85, &tm);
    if (ret != SUCCESS) {
        fprintf(stderr, "find_route failed\n");
        exit(1);
    }
    
    return 0;
}

int test_protobuf()
{
    JoinRoomReq req;
    req.set_room_id(1);
    
    int size = req.ByteSize();
    void *buf = malloc(size);
    if (!req.SerializeToArray(buf, size)) {
        fprintf(stderr, "SerializeToArray failed\n");
        exit(1);
    }
    
    if (!req.ParseFromArray(buf, size)) {
        fprintf(stderr, "ParseFromArray failed\n");
        exit(1);
    }
    
    free(buf);
    return 0;
}

int main()
{
    test_tile_map();
    test_protobuf();
    return 0;
}
