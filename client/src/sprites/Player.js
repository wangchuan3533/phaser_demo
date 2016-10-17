import Phaser from 'phaser'
import Entity from './Entity'
import {Direction, TILE_SIZE, TILE_SHIFT_BITS, Latency} from '../const'
import {Action, Message, MessageType, Protocols, decode} from '../protocol'
const speed = 6 << TILE_SHIFT_BITS

export default class Player extends Entity {
  constructor(opts) {
    opts.asset = opts.asset || 'dude'
    super(opts)
    
    this.animations.add('left', [0, 1, 2, 3], 10, true)
    this.animations.add('right', [5, 6, 7, 8], 10, true)
    
    this.origScale = 0.7
    this.scale.setTo(this.origScale, this.origScale)
    
    this.graph = opts.graph
    this.map = opts.map
    this.index = opts.index
    this.offset = opts.offset
    this.edge = this.map.getEdge(this.index)
    
    this.now = this.game.time.time
    this.checkpoint = 0
    this.actions = []
    this.tick = 0
  }
  
  update() {
    this.tick++
    const dt = this.game.time.time - this.now
    const delta = speed * dt / 1000
    this.now = this.game.time.time
    
    if (this.tick % 2 == 0 && this.actions.length > 0) this.sendAction()
    
    if (this.offset < this.edge.length) {
      if (this.direction == Direction.opposite(this.edge.direction)) {
        this.index ^= 1
        this.edge = this.map.getEdge(this.index)
        this.offset = this.edge.length - this.offset
        this.direction = -1
      }
      this.offset += delta
    } else {
      const vertex = this.map.getVertex(this.edge.dst)
      if (Direction.valid(this.direction) && vertex.idx_out_edges[this.direction] >= 0) {
        this.index = vertex.idx_out_edges[this.direction]
        this.offset = this.offset + delta - this.edge.length
        this.edge = this.map.getEdge(this.index)
        this.direction = -1
      } else if (vertex.idx_out_edges[this.edge.direction] >= 0) {
        this.index = vertex.idx_out_edges[this.edge.direction]
        this.offset = this.offset + delta - this.edge.length
        this.edge = this.map.getEdge(this.index)
      }
    }
  }
  
  move(direction) {
    if (direction == this.direction || direction == this.edge.direction) return
    this.direction = direction
    this.addAction()
  }

  addAction() {
    const elapsed = Math.floor(Date.now() + this.game.transport.offset - this.game.transport.start_time)
    const id = this.checkpoint++
    const direction = this.direction
    const index = this.index
    const offset = Math.floor(this.offset)
    const action = new Action({id, direction, index, offset, elapsed})
    this.actions.push(action)
    console.log({id, direction, index, offset, elapsed})
  }
  
  sendAction() {
    const {ActionReq} = Protocols
    const actions = this.actions.slice(0).reverse()
    const elapsed = Math.floor(Date.now() + this.game.transport.offset - this.game.transport.start_time)
    const action_req = new ActionReq({actions, elapsed})
    const type = MessageType.ACTION_REQ
    const message = new Message({type, action_req})
    this.game.transport.send(message)
  }
  
  action_res(message) {
    const id = message.id
    this.actions = this.actions.filter(action => action.id > id)
  }
}
