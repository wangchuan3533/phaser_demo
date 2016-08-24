export default class Graph {
  constructor(data) {
    this.edges = new Uint32Array(data)
    this.numEdges = this.edges.length / 3
    
    this.graph = {}
    for (let i = 0; i < this.numEdges; i++) {
      const edge = this.getEdge(i)
      this.graph[edge.src] = this.graph[edge.src] || {}
      this.graph[edge.src][edge.direction] = edge.dst
    }
  }
  
  getEdge(index) {
    const src = this.edges[index * 3]
    const dst = this.edges[index * 3 + 1]
    const direction =  this.edges[index * 3 + 2]
    return {src, dst, direction}
  }
  
  randomEdge() {
    return this.getEdge(Math.floor(Math.random() * this.numEdges))
  }
  
  lookup(src, direction) {
    if (this.graph[src] && this.graph[src][direction]) {
      return this.graph[src][direction]
    }
    return -1
  }
  
}
