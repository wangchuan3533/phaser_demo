#ifndef _TILE_MAP_H_
#define _TILE_MAP_H_
#include <stdint.h>

typedef struct vertex {
    uint32_t x;
    uint32_t y;
    int32_t idx_out_edges[4];
} vertex_t;

typedef struct edge {
    uint32_t src;
    uint32_t dst;
    uint32_t direction;
    uint32_t length;
} edge_t;

typedef struct route_node {
    uint32_t distance;
    uint32_t next;
} route_node_t;

typedef struct tile_map {
    uint32_t width;
    uint32_t height;
    
    vertex_t *vertices;
    uint32_t n_vertices;
    
    edge_t *edges;
    uint32_t n_edges;
    
    route_node_t *route_table;
} tile_map_t;

int parse_tile_map(const char *tmx_file_name, tile_map_t *tm);
int find_route(int src, int dst, tile_map_t *tm);
int save_tile_map(const char *tmx_file_name, tile_map_t *tm);
int load_tile_map(const char *tile_map_file_name, tile_map_t *tm);
int release_tile_map(tile_map_t *tm);
#endif
