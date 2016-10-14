#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tmx.h>
#include "common.h"
#include "tile_map.h"
#include <fstream>

#define PATH_LAYER_NAME "layer2"
#define PROP_NAME "tileType"
#define PROP_VALUE "1"
#define INF 0xffffffff
#define XY2INDEX(x, y, w) ((y) * (w) + (x))
#define IS_ROAD(x, y, w, h, d) ((x) >= 0 && (x) <= (w) && (y) >= 0 && (y) <= (h) && (d)[XY2INDEX((x), (y), (w))] == TILE_TYPE_ROAD)
#define IS_WALL(x, y, w, h, d) (!IS_ROAD((x), (y), (w), (h), (d)))
#define IS_INTERMEDIATE(x, y, w, h, d) ((IS_ROAD((x), (y), (w), (h), (d)) && (\
  (IS_ROAD((x) - 1, (y), (w), (h), (d)) && IS_ROAD((x) + 1, (y), (w), (h), (d)) && IS_WALL((x), (y) - 1, (w), (h), (d)) && IS_WALL((x), (y) + 1, (w), (h), (d))) ||\
  (IS_WALL((x) - 1, (y), (w), (h), (d)) && IS_WALL((x) + 1, (y), (w), (h), (d)) && IS_ROAD((x), (y) - 1, (w), (h), (d)) && IS_ROAD((x), (y) + 1, (w), (h), (d)))\
)))

static int prop_get_tile_type(tmx_property *prop)
{
    while (prop != NULL) {
        if ((!strncmp(prop->name, PROP_NAME, strlen(PROP_NAME))) && (!strncmp(prop->value, PROP_VALUE, strlen(PROP_VALUE)))) {
            return TILE_TYPE_ROAD;
        }
    }
    return TILE_TYPE_WALL;
}

static bool parse_tmx(const char *tmx_file_name, int *width, int *height, int **tiles)
{
    tmx_map *map;
    tmx_layer *layer;
    tmx_tile *tile;
    int i, j, total;
    char pathgids[1024];
    
    map = tmx_load(tmx_file_name);
    if (!map) {
        fprintf(stderr, "load tmx file %s failed\n", tmx_file_name);
        return false;
    }
    
    *width = map->width;
    *height = map->height;
    total = (*width) * (*height);
    *tiles = (int *)malloc(sizeof(int) * total);
    
    // cache gid => tile type
    memset(pathgids, 0xff, sizeof(pathgids));
    
    for (layer = map->ly_head; layer != NULL; layer = layer->next) {
        if (strncmp(layer->name, PATH_LAYER_NAME, strlen(PATH_LAYER_NAME))) continue;
        for (i = 0; i < total; i++) {
            j = layer->content.gids[i];
            if (pathgids[j] < 0) {
                tile = tmx_get_tile(map, j);
                if (tile != NULL) {
                    pathgids[j] = (char)prop_get_tile_type(tile->properties);
                } else {
                    pathgids[j] = TILE_TYPE_WALL;
                }
            }
            (*tiles)[i] = pathgids[j];
        }
        break;
    }
    
    return true;
}


bool tile_map::load_from_tmx(const char *tmx_file_name)
{
    int i, j, k, x, y, offset, offset1, offset2, offset3, width, height, total, *tiles = NULL;
    int32_t *idx_vertices = NULL;
    bool ret;
    
    ret = parse_tmx(tmx_file_name, &width, &height, &tiles);
    if (ret != true) {
        fprintf(stderr, "parse tmx file %s failed\n", tmx_file_name);
        return false;
    }
    
    _width = width;
    _height = height;
    total = width * height;
    
    // parse vertices
    _vertices = (vertex_t *)malloc(sizeof(vertex_t) * total);
    _n_vertices = 0;
    idx_vertices = (int32_t *)malloc(sizeof(int32_t) * total);
    memset(idx_vertices, 0xff, sizeof(int32_t) * total);
    
    for (i = 0; i < total; i++) {
        if (tiles[i] == TILE_TYPE_WALL) continue;
        x = i % width;
        y = i / width;
        if (IS_INTERMEDIATE(x, y, width, height, tiles)) continue;
        _vertices[_n_vertices].x = x;
        _vertices[_n_vertices].y = y;
        _vertices[_n_vertices].idx_out_edges[0] = -1;
        _vertices[_n_vertices].idx_out_edges[1] = -1;
        _vertices[_n_vertices].idx_out_edges[2] = -1;
        _vertices[_n_vertices].idx_out_edges[3] = -1;
        idx_vertices[i] = _n_vertices++;
    }
    _vertices = (vertex_t *)realloc(_vertices, sizeof(vertex_t) * _n_vertices);
    
    // parse edges
    _edges = (edge_t *)malloc(sizeof(edge_t) * _n_vertices * _n_vertices);
    _n_edges = 0;
    for (i = 0; i < _n_vertices; i++) {
        x = _vertices[i].x;
        y = _vertices[i].y;
        
        // left
        for (j = x - 1; IS_INTERMEDIATE(j, y, width, height, tiles); j--) ;
        if (IS_ROAD(j, y, width, height, tiles)) {
            offset = XY2INDEX(j, y, width);
            k = idx_vertices[offset];
            
            // left direction edge
            _edges[_n_edges].src = i;
            _edges[_n_edges].dst = k;
            _edges[_n_edges].direction = DIRECTION_LEFT;
            _edges[_n_edges].length = (x - j) << TILE_SHIFT_BITS;
            _vertices[i].idx_out_edges[DIRECTION_LEFT] = _n_edges;
            _n_edges++;
            
            // right direction edge
            _edges[_n_edges].src = k;
            _edges[_n_edges].dst = i;
            _edges[_n_edges].direction = DIRECTION_RIGHT;
            _edges[_n_edges].length = (x - j) << TILE_SHIFT_BITS;
            _vertices[k].idx_out_edges[DIRECTION_RIGHT] = _n_edges;
            _n_edges++;
        }
        
        // up
        for (j = y - 1; IS_INTERMEDIATE(x, j, width, height, tiles); j--) ;
        if (IS_ROAD(x, j, width, height, tiles)) {
            offset = XY2INDEX(x, j, width);
            k = idx_vertices[offset];
            
            // up direction edge
            _edges[_n_edges].src = i;
            _edges[_n_edges].dst = k;
            _edges[_n_edges].direction = DIRECTION_UP;
            _edges[_n_edges].length = (y - j) << TILE_SHIFT_BITS;
            _vertices[i].idx_out_edges[DIRECTION_UP] = _n_edges;
            _n_edges++;
            
            // down direction edge
            _edges[_n_edges].src = k;
            _edges[_n_edges].dst = i;
            _edges[_n_edges].direction = DIRECTION_DOWN;
            _edges[_n_edges].length = (y - j) << TILE_SHIFT_BITS;
            _vertices[k].idx_out_edges[DIRECTION_DOWN] = _n_edges;
            _n_edges++;
        }
    }
    
    // floyd warshall init
    _route_table = (route_node_t *)malloc(sizeof(route_node_t) * _n_vertices * _n_vertices);
    memset(_route_table, 0xff, sizeof(route_node_t) * _n_vertices * _n_vertices);
    for (i = 0; i < _n_vertices; i++) {
        offset = i * _n_vertices + i;
        _route_table[offset].distance = 0;
        _route_table[offset].next = i;
    }
    
    for (i = 0; i < _n_edges; i++) {
        offset = _edges[i].src * _n_vertices + _edges[i].dst;
        _route_table[offset].distance = _edges[i].length;
        _route_table[offset].next = _edges[i].dst;
    }
    
    // floyd warshall iteration
    for (k = 0; k < _n_vertices; k++) {
        for (i = 0; i < _n_vertices; i++) {
            for (j = 0; j < _n_vertices; j++) {
                offset1 = i * _n_vertices + k;
                offset2 = k * _n_vertices + j;
                offset3 = i * _n_vertices + j;
                
                if (_route_table[offset1].distance != INF && _route_table[offset2].distance != INF
                    && _route_table[offset3].distance > _route_table[offset1].distance + _route_table[offset2].distance) {
                    _route_table[offset3].distance = _route_table[offset1].distance + _route_table[offset2].distance;
                    _route_table[offset3].next = _route_table[offset1].next;
                }
            }
        }
    }
    
    // free
    free(idx_vertices);
    free(tiles);
    
    return true;
}

bool tile_map::save(const char *tile_map_file_name)
{
    FILE *output_fp = fopen(tile_map_file_name, "w");
    if (output_fp == NULL) {
        fprintf(stderr, "file %s open failed\n", tile_map_file_name);
        return false;
    }
    
    // width
    if (fwrite(&(_width), sizeof(_width), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite width failed\n");
        fclose(output_fp);
        return false;
    };
    
    // height
    if (fwrite(&(_height), sizeof(_height), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite height failed\n");
        fclose(output_fp);
        return false;
    };
    
    // n_vertices
    if (fwrite(&(_n_vertices), sizeof(_n_vertices), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite n_vertices failed\n");
        fclose(output_fp);
        return false;
    };
    
    // vertices
    if (fwrite(_vertices, sizeof(vertex_t), _n_vertices, output_fp) != _n_vertices) {
        fprintf(stderr, "fwrite vertices failed\n");
        fclose(output_fp);
        return false;
    };
    
    // n_edges
    if (fwrite(&(_n_edges), sizeof(_n_edges), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite n_edges failed\n");
        fclose(output_fp);
        return false;
    };
    
    // edges
    if (fwrite(_edges, sizeof(edge_t), _n_edges, output_fp) != _n_edges) {
        fprintf(stderr, "fwrite edges failed\n");
        fclose(output_fp);
        return false;
    };
    
    // route_table
    if (fwrite(_route_table, sizeof(route_node_t), _n_vertices * _n_vertices, output_fp) != _n_vertices * _n_vertices) {
        fprintf(stderr, "fwrite route_table failed\n");
        fclose(output_fp);
        return false;
    };
    
    fclose(output_fp);
    return true;
}

bool tile_map::save_as_pb(const char *pb_file_name)
{
    demo::protocol::TileMap tm;
    tm.set_width(_width);
    tm.set_height(_height);
    
    // vertices
    for (int i = 0; i < _n_vertices; i++) {
        demo::protocol::Vertex *vertex = tm.add_vertices();
        vertex->set_x(_vertices[i].x);
        vertex->set_y(_vertices[i].y);
        vertex->add_idx_out_edges(_vertices[i].idx_out_edges[0]);
        vertex->add_idx_out_edges(_vertices[i].idx_out_edges[1]);
        vertex->add_idx_out_edges(_vertices[i].idx_out_edges[2]);
        vertex->add_idx_out_edges(_vertices[i].idx_out_edges[3]);
    }
    
    // edges
    for (int i = 0; i < _n_edges; i++) {
        demo::protocol::Edge *edge = tm.add_edges();
        edge->set_src(_edges[i].src);
        edge->set_dst(_edges[i].dst);
        edge->set_direction(_edges[i].direction);
        edge->set_length(_edges[i].length);
    }
    
    // routes
    for (int i = 0, total = _n_vertices * _n_vertices; i < total; i++) {
        demo::protocol::RouteNode *node = tm.add_routes();
        node->set_distance(_route_table[i].distance);
        node->set_next(_route_table[i].next);
    }
    
    std::fstream output_file(pb_file_name, std::ios::out | std::ios::trunc | std::ios::binary);
    if (!output_file) {
        fprintf(stderr, "fstream output to %sfailed\n", pb_file_name);
        return false;
    }
    if (!tm.SerializeToOstream(&output_file)) {
        fprintf(stderr, "SerializeToOstream failed\n");
        return false;
    };
    output_file.close();
    return true;
}
bool tile_map::load_from_pb(const char *pb_file_name)
{
    demo::protocol::TileMap tm;
    std::fstream input_file(pb_file_name, std::ios::in | std::ios::binary);
    if (!input_file) {
        fprintf(stderr, "fstream input from %s failed\n", pb_file_name);
        return false;
    }
    
    if (!tm.ParseFromIstream(&input_file)) {
        fprintf(stderr, "ParseFromIstream failse\n");
        return false;
    }
    
    _width = tm.width();
    _height = tm.height();
    
    // vertices
    _n_vertices = tm.vertices_size();
    _vertices = (vertex_t *)malloc(sizeof(vertex_t) * _n_vertices);
    for (int i = 0; i < _n_vertices; i++) {
        const demo::protocol::Vertex &vertex = tm.vertices(i);
        _vertices[i].x = vertex.x();
        _vertices[i].y = vertex.y();
        _vertices[i].idx_out_edges[0] = vertex.idx_out_edges(0);
        _vertices[i].idx_out_edges[1] = vertex.idx_out_edges(1);
        _vertices[i].idx_out_edges[2] = vertex.idx_out_edges(2);
        _vertices[i].idx_out_edges[3] = vertex.idx_out_edges(3);
    }
    
    // edges
    _n_edges = tm.edges_size();
    _edges = (edge_t *)malloc(sizeof(edge_t) * _n_edges);
    for (int i = 0; i < _n_edges; i++) {
        const demo::protocol::Edge &edge = tm.edges(i);
        _edges[i].src = edge.src();
        _edges[i].dst = edge.dst();
        _edges[i].direction = edge.direction();
        _edges[i].length = edge.length();
    }
    
    // routes
    int total = _n_vertices * _n_vertices;
    _route_table = (route_node_t *)malloc(sizeof(route_node_t) * total);
    for (int i = 0; i < total; i++) {
        const demo::protocol::RouteNode &node = tm.routes(i);
        _route_table[i].distance = node.distance();
        _route_table[i].next = node.next();
    }
    
    input_file.close();
    return true;
}

bool tile_map::load(const char *tile_map_file_name)
{
    FILE *input_fp = fopen(tile_map_file_name, "r");
    if (input_fp == NULL) {
        fprintf(stderr, "file %s open failed\n", tile_map_file_name);
        return false;
    }
    
    if (fread(&(_width), sizeof(_width), 1, input_fp) != 1) {
        fprintf(stderr, "fread width failed\n");
        fclose(input_fp);
        return false;
    }
    
    if (fread(&(_height), sizeof(_height), 1, input_fp) != 1) {
        fprintf(stderr, "fread height failed\n");
        fclose(input_fp);
        return false;
    }
    
    if (fread(&(_n_vertices), sizeof(_n_vertices), 1, input_fp) != 1) {
        fprintf(stderr, "fread n_vertices failed\n");
        fclose(input_fp);
        return false;
    }
    
    _vertices = (vertex_t *)malloc(sizeof(vertex_t) * _n_vertices);
    if (fread(_vertices, sizeof(vertex_t), _n_vertices, input_fp) != _n_vertices) {
        fprintf(stderr, "fread vertices failed\n");
        fclose(input_fp);
        clear_memory();
        return false;
    }
    
    if (fread(&(_n_edges), sizeof(_n_edges), 1, input_fp) != 1) {
        fprintf(stderr, "fread n_edges failed\n");
        fclose(input_fp);
        clear_memory();
        return false;
    }
    
    _edges = (edge_t *)malloc(sizeof(edge_t) * _n_edges);
    if (fread(_edges, sizeof(edge_t), _n_edges, input_fp) != _n_edges) {
        fprintf(stderr, "fread edges failed\n");
        fclose(input_fp);
        clear_memory();
        return false;
    }
    
    _route_table = (route_node_t *)malloc(sizeof(route_node_t) * _n_vertices * _n_vertices);
    if (fread(_route_table, sizeof(route_node_t), _n_vertices * _n_vertices, input_fp) != _n_vertices * _n_vertices) {
        fprintf(stderr, "fread route_table failed\n");
        fclose(input_fp);
        clear_memory();
        return false;
    }
    
    fclose(input_fp);
    return true;
}


void tile_map::pos_to_xy(const position_t &pos, double &x, double &y)
{
    edge_t *edge = &_edges[pos.index];
    vertex_t *src = &_vertices[edge->src];
    x = src->x;
    y = src->y;
    switch(edge->direction) {
    case DIRECTION_UP:
        y -= pos.offset / (1 << TILE_SHIFT_BITS);
        break;
    case DIRECTION_DOWN:
        y += pos.offset / (1 << TILE_SHIFT_BITS);
        break;
    case DIRECTION_LEFT:
        x -= pos.offset / (1 << TILE_SHIFT_BITS);
        break;
    case DIRECTION_RIGHT:
        x += pos.offset / (1 << TILE_SHIFT_BITS);
        break;
    }
}

bool tile_map::find_route(int src, int dst)
{
    route_node_t *node;
    
    while (src != dst) {
        node = &(_route_table[src * _n_vertices + dst]);
        printf("[%d, %d, %d]\n", _vertices[src].x, _vertices[src].y, node->distance);
        src = node->next;
    }
    return true;
}
