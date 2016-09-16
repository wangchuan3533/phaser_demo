import Phaser from 'phaser'
import Entity from './Entity'
import {Direction, TILE_SIZE, Latency} from '../const'
import {Message, MessageType, Protocols, decode} from '../protocol'
const speed = 6

export default class Player extends Entity {
  constructor(opts) {
    opts.asset = opts.asset || 'dude'
    super(opts)
    
    this.animations.add('left', [0, 1, 2, 3], 10, true)
    this.animations.add('right', [5, 6, 7, 8], 10, true)
    
    this.origScale = 0.8
    this.scale.setTo(this.origScale, this.origScale)
    
    this.graph = opts.graph
    this.map = opts.map
    this.src = opts.src
    this.dst = opts.dst
    this.offset = opts.offset
    
    this.now = this.game.time.time
    this.checkpoint = 0
  }
  
  update() {
    const dt = this.game.time.time - this.now
    const delta = Math.floor(speed * dt * 0x10000 / 1000)
    this.now = this.game.time.time
    
    if (this.src >= 0 && this.dst >= 0) {
      this.offset += delta
      
      if (this.offset >= this.offsetMax) {
        const dst1 = this.graph.lookup(this.dst, this.nextDirection)
        const dst2 = this.graph.lookup(this.dst, this.direction)
        if (dst1 >= 0) {
          this.src = this.dst
          this.dst = dst1
          this.offset = this.offset - this.offsetMax
          this.offsetMax = this.map.edgeDistance(this.src, this.dst)
          this.direction = this.nextDirection
          this.nextDirection = -1
        } else if(dst2 >= 0) {
          this.src = this.dst
          this.dst = dst2
          this.offset = this.offset - this.offsetMax
          this.offsetMax = this.map.edgeDistance(this.src, this.dst)
        } else {
          this.src = this.dst
          this.dst = -1
          this.direction = -1
          this.nextDirection = -1
          this.offset = 0
        }
        this.sendCheckpoint()
      } else if (this.now - this.lastCheckpoint > 50) {
        this.sendCheckpoint()
      }
    }
  }
  
  move(direction) {
    if (this.dst == -1) {
      const dst = this.graph.lookup(this.src, direction)
      if (dst >= 0) {
        this.dst = dst
        this.offset = 0
        this.offsetMax = this.map.edgeDistance(this.src, this.dst)
        this.direction = direction
        this.nextDirection = -1
      }
    } else if (direction == Direction.opposite(this.direction)) {
      const dst = this.src
      const src = this.dst
      this.src = src
      this.dst = dst
      this.offset = this.offsetMax - this.offset
      this.direction = direction
      this.nextDirection = -1
      this.sendCheckpoint()
    } else if (this.direction != direction && this.nextDirection!= direction) {
      this.nextDirection = direction
    }
  }

  sendCheckpoint() {
    const {CheckpointReq} = Protocols
    const elapsed = Math.floor(this.now - this.game.transport.start_time)
    const id = this.checkpoint++
    const {src, dst} = this
    const offset = Math.floor(this.offset)
    const latency = this.game.transport.latency
    const req = new CheckpointReq({id, src, dst, offset, elapsed, latency})
    this.game.transport.send(MessageType.CHECKPOINT_REQ, req)
    this.lastCheckpoint = this.now
  }
}
