export const TILE_SIZE = 20
export const Direction = {
  LEFT: 1,
  RIGHT: 2,
  UP: 3,
  DOWN: 4,
  
  direction2vector: function (direction) {
    switch (direction) {
      case this.LEFT:
        return {x: -1, y: 0}
      case this.RIGHT:
        return {x: 1, y: 0}
      case this.UP:
        return {x: 0, y: -1}
      case this.DOWN:
        return {x: 0, y: 1}
      default:
        return {x: 0, y: 0}
    }
  },
  
  opposite: function (direction) {
    switch (direction) {
      case this.LEFT:
        return this.RIGHT
      case this.RIGHT:
        return this.LEFT
      case this.UP:
        return this.DOWN
      case this.DOWN:
        return this.UP
      default:
        return -1
    }
  }
}

export const Latency = {
  MIN: 50,
  MAX: 100,
  random: function() {
    return this.MIN + Math.random() * (this.MAX - this.MIN)
  }
}
