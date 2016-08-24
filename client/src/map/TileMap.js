export default class TileMap {
  constructor(data) {
    const uint32array = new Uint32Array(data)
    this.width = uint32array[0]
    this.height = uint32array[1]
    this.numNodes = uint32array[2]
    this.nodeIdMap = {}
    for (let i = 0; i < this.numNodes; i++) {
      this.nodeIdMap[uint32array[i + 3]] = i
    }
    
    this.directions = new Uint8Array(uint32array.slice(this.numNodes + 3).buffer)
  }
  
  isWall(x, y) {
    if (x < 0 || x >= this.width || y < 0 || y >= this.height) {
      return true
    }
    
    const index = y * this.width + x
    return this.nodeIdMap[index] == null
  }
  
  isRoad(x, y) {
    return !this.isWall(x, y)
  }
  
  xy2index(x, y) {
    return this.width * y + x;
  }

  index2xy(index) {
    const x = index % this.width;
    const y = Math.floor(index / this.width);
    return {x, y};
  }
  
  edge2xy(src, dst, offset) {
    const p1 = this.index2xy(src)
    const p2 = this.index2xy(dst)
    const dx = p2.x - p1.x
    const dy = p2.y - p1.y
    
    let {x, y} = p1
    if (dx > 0) {
      x += offset
    } else if (dx < 0) {
      x -= offset
    } else if (dy > 0) {
      y += offset
    } else if (dy < 0) {
      y -= offset
    }
    
    return {x, y}
  }
  
  edgeDistance(src, dst) {
    const p1 = this.index2xy(src)
    const p2 = this.index2xy(dst)
    const dx = p2.x - p1.x
    const dy = p2.y - p1.y
    
    return Math.abs(dx) + Math.abs(dy)
  }
  
  lookup(index1, index2) {
    const node1 = this.nodeIdMap[index1]
    const node2 = this.nodeIdMap[index2]
    
    if (node1 >= 0 && node2 >= 0) {
      return this.directions[node1 * this.numNodes + node2]
    }
    return false
  }
  
  randomPosition() {
    const nodeId = Math.floor(this.numNodes * Math.random())
    const index = Object.keys(this.nodeIdMap)[nodeId]
    return this.index2xy(index)
  }
}
