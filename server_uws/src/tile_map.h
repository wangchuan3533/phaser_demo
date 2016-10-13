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

typedef struct position {
    uint32_t index;
    uint32_t offset;
} position_t;

/*
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
*/

class tile_map {
public:
    bool load(const char *tile_map_file_name);
    bool save(const char *tile_map_file_name);
    bool save_as_pb(const char *pb_file_name);
    bool load_from_pb(const char *pb_file_name);
    bool load_from_tmx(const char *tmx_file_name);
    bool find_route(int src, int dst);
    void pos_to_xy(const position_t &pos, double &x, double &y);
    
    inline position_t random_position()
    {
        position_t pos;
        pos.index = random() % _n_edges;
        pos.offset = 0;
        
        return pos;
    }
    
    inline void clear_memory()
    {
        if (_vertices) {
            free(_vertices);
            _vertices = NULL;
        }
        
        if (_edges) {
            free(_edges);
            _edges = NULL;
        }
        
        if (_route_table) {
            free(_route_table);
            _route_table = NULL;
        }
    }
    
    edge_t *get_edge(uint32_t index)
    {
        return &_edges[index];
    }
    
    vertex_t *get_vertex(uint32_t index)
    {
        return &_vertices[index];
    }
    
    route_node_t *get_route_node(uint32_t src, uint32_t dst)
    {
        uint32_t offset = src * _n_vertices + dst;
        return &_route_table[offset];
    }
    
private:
    uint32_t _width;
    uint32_t _height;
    
    vertex_t *_vertices;
    uint32_t _n_vertices;
    
    edge_t *_edges;
    uint32_t _n_edges;
    
    route_node_t *_route_table;
};
#endif
