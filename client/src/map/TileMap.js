import {Direction, TILE_SHIFT_BITS} from '../const'
const VERTEX_SIZE = 6
const EDGE_SIZE = 4
const ROUTE_NODE_SIZE = 2

export default class TileMap {
  constructor(data) {
    const uint32array = new Uint32Array(data)
    let offset = 0
    
    this.width = uint32array[offset++]
    this.height = uint32array[offset++]
    
    this.n_vertices = uint32array[offset++]
    this.vertices = uint32array.slice(offset, offset + VERTEX_SIZE * this.n_vertices)
    offset += VERTEX_SIZE * this.n_vertices
    
    this.n_edges = uint32array[offset++]
    this.edges = uint32array.slice(offset, offset + EDGE_SIZE * this.n_edges)
    offset += EDGE_SIZE * this.n_edges
    
    this.route_table = uint32array.slice(offset)
  }
  
  getVertex(index) {
    let offset = index * VERTEX_SIZE
    const x = this.vertices[offset++]
    const y = this.vertices[offset++]
    const idx_out_edges = new Int32Array(this.vertices.slice(offset, offset + 4).buffer)
    
    return {x, y, idx_out_edges}
  }
  
  getEdge(index) {
    let offset = index * EDGE_SIZE
    const src = this.edges[offset++]
    const dst = this.edges[offset++]
    const direction = this.edges[offset++]
    const length = this.edges[offset++]
    
    return {src, dst, direction, length}
  }
  
  getRouteNode(index) {
    let offset = index * ROUTE_NODE_SIZE
    const distance = this.route_table[offset++]
    const next = this.route_table[offset++]
    return {distance, next}
  }
  
  pos_to_xy(index, offset) {
    const edge = this.getEdge(index)
    const vertex = this.getVertex(edge.src)
    let x = vertex.x
    let y = vertex.y
    switch (edge.direction) {
      case Direction.DIRECTION_UP:
        y -= offset / (1 << TILE_SHIFT_BITS)
        break
      case Direction.DIRECTION_DOWN:
        y += offset / (1 << TILE_SHIFT_BITS)
        break
      case Direction.DIRECTION_LEFT:
        x -= offset / (1 << TILE_SHIFT_BITS)
        break
      case Direction.DIRECTION_RIGHT:
        x += offset / (1 << TILE_SHIFT_BITS)
        break
    }
    return {x, y}
  }
  
}
