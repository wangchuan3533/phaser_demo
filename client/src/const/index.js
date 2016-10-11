export const TILE_SIZE = 30
export const TileType = {
  TILE_TYPE_WALL: 0,
  TILE_TYPE_ROAD: 1
}
export const TILE_SHIFT_BITS = 10
export const Direction = {
  DIRECTION_RIGHT: 0,
  DIRECTION_DOWN:  1,
  DIRECTION_LEFT:  2,
  DIRECTION_UP:    3,
  
  opposite: function (direction) {
    switch (direction) {
      case this.DIRECTION_LEFT:
        return this.DIRECTION_RIGHT
      case this.DIRECTION_RIGHT:
        return this.DIRECTION_LEFT
      case this.DIRECTION_UP:
        return this.DIRECTION_DOWN
      case this.DIRECTION_DOWN:
        return this.DIRECTION_UP
      default:
        throw 'invalid direction' + direction
    }
  },
  
  valid: function (direction) {
    return direction == 0 || direction == 1 || direction == 2 || direction == 3
  }
}

export const Latency = {
  MIN: 100,
  MAX: 100,
  random: function() {
    return this.MIN + Math.random() * (this.MAX - this.MIN)
  }
}

export const TICK = 60
