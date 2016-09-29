#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tmx.h>
#include "common.h"
#include "tile_map.h"

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

int prop_get_tile_type(tmx_property *prop)
{
    while (prop != NULL) {
        if ((!strncmp(prop->name, PROP_NAME, strlen(PROP_NAME))) && (!strncmp(prop->value, PROP_VALUE, strlen(PROP_VALUE)))) {
            return TILE_TYPE_ROAD;
        }
    }
    return TILE_TYPE_WALL;
}

int parse_tmx(const char *tmx_file_name, int *width, int *height, int **tiles)
{
    tmx_map *map;
    tmx_layer *layer;
    tmx_tile *tile;
    int i, j, total;
    char pathgids[1024];
    
    map = tmx_load(tmx_file_name);
    if (!map) {
        fprintf(stderr, "load tmx file %s failed\n", tmx_file_name);
        return FAILURE;
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
    
    return SUCCESS;
}

int parse_tile_map(const char *tmx_file_name, tile_map_t *tm)
{
    int i, j, k, x, y, offset, offset1, offset2, offset3, length, width, height, total, *tiles, ret;
    int32_t *idx_vertices;
    
    ret = parse_tmx(tmx_file_name, &width, &height, &tiles);
    if (ret != SUCCESS) {
        fprintf(stderr, "parse tmx file %s failed\n", tmx_file_name);
        return FAILURE;
    }
    
    tm->width = width;
    tm->height = height;
    total = width * height;
    
    // parse vertices
    tm->vertices = (vertex_t *)malloc(sizeof(vertex_t) * total);
    tm->n_vertices = 0;
    idx_vertices = (int32_t *)malloc(sizeof(int32_t) * total);
    memset(idx_vertices, 0xff, sizeof(int32_t) * total);
    
    for (i = 0; i < total; i++) {
        if (tiles[i] == TILE_TYPE_WALL) continue;
        x = i % width;
        y = i / width;
        if (IS_INTERMEDIATE(x, y, width, height, tiles)) continue;
        tm->vertices[tm->n_vertices].x = x;
        tm->vertices[tm->n_vertices].y = y;
        tm->vertices[tm->n_vertices].idx_out_edges[0] = -1;
        tm->vertices[tm->n_vertices].idx_out_edges[1] = -1;
        tm->vertices[tm->n_vertices].idx_out_edges[2] = -1;
        tm->vertices[tm->n_vertices].idx_out_edges[3] = -1;
        idx_vertices[i] = tm->n_vertices++;
    }
    tm->vertices = (vertex_t *)realloc(tm->vertices, sizeof(vertex_t) * tm->n_vertices);
    
    // parse edges
    tm->edges = (edge_t *)malloc(sizeof(edge_t) * tm->n_vertices * tm->n_vertices);
    tm->n_edges = 0;
    for (i = 0; i < tm->n_vertices; i++) {
        x = tm->vertices[i].x;
        y = tm->vertices[i].y;
        
        // left
        for (j = x - 1; IS_INTERMEDIATE(j, y, width, height, tiles); j--) ;
        if (IS_ROAD(j, y, width, height, tiles)) {
            offset = XY2INDEX(j, y, width);
            k = idx_vertices[offset];
            
            // left direction edge
            tm->edges[tm->n_edges].src = i;
            tm->edges[tm->n_edges].dst = k;
            tm->edges[tm->n_edges].direction = DIRECTION_LEFT;
            tm->edges[tm->n_edges].length = x - j;
            tm->vertices[i].idx_out_edges[DIRECTION_LEFT] = tm->n_edges;
            tm->n_edges++;
            
            // right direction edge
            tm->edges[tm->n_edges].src = k;
            tm->edges[tm->n_edges].dst = i;
            tm->edges[tm->n_edges].direction = DIRECTION_RIGHT;
            tm->edges[tm->n_edges].length = x - j;
            tm->vertices[k].idx_out_edges[DIRECTION_RIGHT] = tm->n_edges;
            tm->n_edges++;
        }
        
        // up
        for (j = y - 1; IS_INTERMEDIATE(x, j, width, height, tiles); j--) ;
        if (IS_ROAD(x, j, width, height, tiles)) {
            offset = XY2INDEX(x, j, width);
            k = idx_vertices[offset];
            
            // up direction edge
            tm->edges[tm->n_edges].src = i;
            tm->edges[tm->n_edges].dst = k;
            tm->edges[tm->n_edges].direction = DIRECTION_UP;
            tm->edges[tm->n_edges].length = y - j;
            tm->vertices[i].idx_out_edges[DIRECTION_UP] = tm->n_edges;
            tm->n_edges++;
            
            // down direction edge
            tm->edges[tm->n_edges].src = k;
            tm->edges[tm->n_edges].dst = i;
            tm->edges[tm->n_edges].direction = DIRECTION_DOWN;
            tm->edges[tm->n_edges].length = y - j;
            tm->vertices[k].idx_out_edges[DIRECTION_DOWN] = tm->n_edges;
            tm->n_edges++;
        }
    }
    
    // floyd warshall init
    tm->route_table = (route_node_t *)malloc(sizeof(route_node_t) * tm->n_vertices * tm->n_vertices);
    memset(tm->route_table, 0xff, sizeof(route_node_t) * tm->n_vertices * tm->n_vertices);
    for (i = 0; i < tm->n_vertices; i++) {
        offset = i * tm->n_vertices + i;
        tm->route_table[offset].distance = 0;
        tm->route_table[offset].next = i;
    }
    
    for (i = 0; i < tm->n_edges; i++) {
        offset = tm->edges[i].src * tm->n_vertices + tm->edges[i].dst;
        tm->route_table[offset].distance = tm->edges[i].length;
        tm->route_table[offset].next = tm->edges[i].dst;
    }
    
    // floyd warshall iteration
    for (k = 0; k < tm->n_vertices; k++) {
        for (i = 0; i < tm->n_vertices; i++) {
            for (j = 0; j < tm->n_vertices; j++) {
                offset1 = i * tm->n_vertices + k;
                offset2 = k * tm->n_vertices + j;
                offset3 = i * tm->n_vertices + j;
                
                if (tm->route_table[offset1].distance != INF && tm->route_table[offset2].distance != INF
                    && tm->route_table[offset3].distance > tm->route_table[offset1].distance + tm->route_table[offset2].distance) {
                    tm->route_table[offset3].distance = tm->route_table[offset1].distance + tm->route_table[offset2].distance;
                    tm->route_table[offset3].next = tm->route_table[offset1].next;
                }
            }
        }
    }
    
    // free
    free(idx_vertices);
    free(tiles);
    
    return SUCCESS;
}

int save_tile_map(const char *tile_map_file_name, tile_map_t *tm)
{
    FILE *output_fp = fopen(tile_map_file_name, "w");
    if (output_fp == NULL) {
        fprintf(stderr, "file %s open failed\n", tile_map_file_name);
        return FAILURE;
    }
    
    // width
    if (fwrite(&(tm->width), sizeof(tm->width), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite width failed\n");
        fclose(output_fp);
        return FAILURE;
    };
    
    // height
    if (fwrite(&(tm->height), sizeof(tm->height), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite height failed\n");
        fclose(output_fp);
        return FAILURE;
    };
    
    // n_vertices
    if (fwrite(&(tm->n_vertices), sizeof(tm->n_vertices), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite n_vertices failed\n");
        fclose(output_fp);
        return FAILURE;
    };
    
    // vertices
    if (fwrite(tm->vertices, sizeof(vertex_t), tm->n_vertices, output_fp) != tm->n_vertices) {
        fprintf(stderr, "fwrite vertices failed\n");
        fclose(output_fp);
        return FAILURE;
    };
    
    // n_edges
    if (fwrite(&(tm->n_edges), sizeof(tm->n_edges), 1, output_fp) != 1) {
        fprintf(stderr, "fwrite n_edges failed\n");
        fclose(output_fp);
        return FAILURE;
    };
    
    // edges
    if (fwrite(tm->edges, sizeof(edge_t), tm->n_edges, output_fp) != tm->n_edges) {
        fprintf(stderr, "fwrite edges failed\n");
        fclose(output_fp);
        return FAILURE;
    };
    
    // route_table
    if (fwrite(tm->route_table, sizeof(route_node_t), tm->n_vertices * tm->n_vertices, output_fp) != tm->n_vertices * tm->n_vertices) {
        fprintf(stderr, "fwrite route_table failed\n");
        fclose(output_fp);
        return FAILURE;
    };
    
    fclose(output_fp);
    return SUCCESS;
}

int load_tile_map(const char *tile_map_file_name, tile_map_t *tm)
{
    FILE *input_fp = fopen(tile_map_file_name, "r");
    if (input_fp == NULL) {
        fprintf(stderr, "file %s open failed\n", tile_map_file_name);
        return FAILURE;
    }
    
    if (fread(&(tm->width), sizeof(tm->width), 1, input_fp) != 1) {
        fprintf(stderr, "fread width failed\n");
        fclose(input_fp);
        return FAILURE;
    }
    
    if (fread(&(tm->height), sizeof(tm->height), 1, input_fp) != 1) {
        fprintf(stderr, "fread height failed\n");
        fclose(input_fp);
        return FAILURE;
    }
    
    if (fread(&(tm->n_vertices), sizeof(tm->n_vertices), 1, input_fp) != 1) {
        fprintf(stderr, "fread n_vertices failed\n");
        fclose(input_fp);
        return FAILURE;
    }
    
    tm->vertices = (vertex_t *)malloc(sizeof(vertex_t) * tm->n_vertices);
    if (fread(tm->vertices, sizeof(vertex_t), tm->n_vertices, input_fp) != tm->n_vertices) {
        fprintf(stderr, "fread vertices failed\n");
        fclose(input_fp);
        return FAILURE;
    }
    
    if (fread(&(tm->n_edges), sizeof(tm->n_edges), 1, input_fp) != 1) {
        fprintf(stderr, "fread n_edges failed\n");
        fclose(input_fp);
        return FAILURE;
    }
    
    tm->edges = (edge_t *)malloc(sizeof(edge_t) * tm->n_edges);
    if (fread(tm->edges, sizeof(edge_t), tm->n_edges, input_fp) != tm->n_edges) {
        fprintf(stderr, "fread edges failed\n");
        fclose(input_fp);
        return FAILURE;
    }
    
    tm->route_table = (route_node_t *)malloc(sizeof(route_node_t) * tm->n_vertices * tm->n_vertices);
    if (fread(tm->route_table, sizeof(route_node_t), tm->n_vertices * tm->n_vertices, input_fp) != tm->n_vertices * tm->n_vertices) {
        fprintf(stderr, "fread route_table failed\n");
        fclose(input_fp);
        return FAILURE;
    }
    
    fclose(input_fp);
    return SUCCESS;
}

int release_tile_map(tile_map_t *tm)
{
    free(tm->vertices);
    free(tm->edges);
    free(tm->route_table);
    return SUCCESS;
}

int find_route(int src, int dst, tile_map_t *tm)
{
    route_node_t *node;
    
    while (src != dst) {
        node = &(tm->route_table[src * tm->n_vertices + dst]);
        printf("[%d, %d, %d]\n", tm->vertices[src].x, tm->vertices[src].y, node->distance);
        src = node->next;
    }
    return 0;
}
