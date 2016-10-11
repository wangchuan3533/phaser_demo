#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "tile_map.h"
#define TEST_TMX_FILE_NAME "data/8.tmx"
#define TEST_TILE_MAP_FILE_NAME "data/8.data"
int test_tile_map()
{
    class tile_map tm;
    int ret;
    
    ret = tm.load_from_tmx(TEST_TMX_FILE_NAME);
    if (ret != true) {
        fprintf(stderr, "parse_tile_map failed\n");
        exit(1);
    }
    
    // save
    ret = tm.save(TEST_TILE_MAP_FILE_NAME);
    if (ret != true) {
        fprintf(stderr, "save_tile_map failed\n");
        exit(1);
    }
    
    tm.clear_memory();
    
    ret = tm.load(TEST_TILE_MAP_FILE_NAME);
    if (ret != true) {
        fprintf(stderr, "save_tile_map failed\n");
        exit(1);
    }
    
    /*
    for (i = 0; i < tm._n_vertices; i++) {
        printf("[vertex] index: %d, position: [%d, %d], edges: [%d, %d, %d, %d]\n",
            i, tm._vertices[i].x, tm._vertices[i].y,
            tm._vertices[i].idx_out_edges[0],
            tm._vertices[i].idx_out_edges[1],
            tm._vertices[i].idx_out_edges[2],
            tm._vertices[i].idx_out_edges[3]
        );
    }
    
    for (i = 0; i < tm._n_edges; i++) {
        printf("[edge] index: %d, src: [%d, %d], dst: [%d, %d], length: %d, direction: %d\n",
            i, tm._vertices[tm._edges[i].src].x, tm._vertices[tm._edges[i].src].y,
            tm._vertices[tm._edges[i].dst].x, tm._vertices[tm._edges[i].dst].y,
            tm._edges[i].length, tm._edges[i].direction);
    }
    
    for (i = 0; i < tm._n_vertices; i++) {
        for (j = 0; j < tm._n_vertices; j++) {
            offset = i * tm._n_vertices + j;
            printf("[routing] from: %d, to: %d, distance: %d, next: %d\n",
                i, j, tm._route_table[offset].distance, tm._route_table[offset].next);
        }
    }
    */
    
    ret = tm.find_route(0, 85);
    if (ret != true) {
        fprintf(stderr, "find_route failed\n");
        exit(1);
    }
    
    printf("sizeof vertex_t is %lu\n", sizeof(vertex_t));
    printf("sizeof edge_t is %lu\n", sizeof(edge_t));
    printf("sizeof route_node_t is %lu\n", sizeof(route_node_t));
    
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
