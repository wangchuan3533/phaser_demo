import Phaser from 'phaser'
import Player from '../sprites/Player'
import Shadow from '../sprites/Shadow'
import {Direction, TILE_SIZE, Latency} from '../const'
import TileMap from '../map/TileMap'
import Graph from '../map/Graph'
import {Message, MessageType, Protocols} from '../protocol'

export default class extends Phaser.State {
  init() {
    this.players = {}
    this.shadows = {}
  }
  preload() {}

  create() {
    // prepare
    const path = this.cache.getBinary('path')
    this.map = new TileMap(path)
    const edge = this.cache.getBinary('edge')
    this.graph = new Graph(edge)
    
    // create roads
    this.createRoads()
    
    // cursors
    this.cursors = this.game.input.keyboard.createCursorKeys()
    
    // join room
    this.joinRoom()
    
    // regiester protocol
    this.game.transport.register('message', this.onMessage.bind(this))
    
    // global functions
    this.edge2xy = (src, dst, offset) => {
      const {x, y} = this.map.edge2xy(src, dst, offset)
      return {x: (x + 0.5) * TILE_SIZE, y: (y + 0.5) * TILE_SIZE}
    }
    
    // debug info
    this.game.time.advancedTiming = true
    const debugInfo = this.add.text(this.world.width - 128, 32, 'debug info', { font: '16px Arial', fill: '#eeeeee', align: 'center' })
    debugInfo.anchor.setTo(0.5, 0.5)
    this.debugInfo = debugInfo
  }
  
  getClientTime() {
    return this.game.time.time - this.game.transport.start_time
  }
  
  createRoads() {
    const border = 0
    const graphics = this.add.graphics(0, 0)
    
    // draw roads
    graphics.lineStyle(1, 0xafafaf, 1)
    graphics.beginFill(0xafafaf, 1)
    
    for (let index in this.map.nodeIdMap) {
      const {x, y} = this.map.index2xy(index)
      const extra = TILE_SIZE >> 1
      graphics.drawRect(x * TILE_SIZE + border - extra, y * TILE_SIZE + border - extra, TILE_SIZE - 2 * border + 2 * extra, TILE_SIZE - 2 * border + 2 * extra)
    }
    
    graphics.endFill()
    this.walls = graphics
  }
  
  createPlayer(opts) {
    const player = new Player({
      ...opts,
      game: this.game,
      map: this.map,
      graph: this.graph,
    })
    
    this.game.add.existing(player)
    this.players[opts.id] = player
    return player
  }
  
  createShadow(opts) {
    const shadow = new Shadow({
      ...opts,
      game: this.game,
      map: this.map,
      graph: this.graph,
    })
    
    this.game.add.existing(shadow)
    this.shadows[opts.id] = shadow
    return shadow
  }
  
  onMessage(message) {
    switch (message.type) {
      case MessageType.JOIN_ROOM_RES:
        for (let i = 0; i < message.entities.length; i++) {
          const entity = message.entities[i]
          const shadow = this.createShadow(entity)
          if (entity.id == message.player_id) {
            const player = this.createPlayer(entity)
            this.player = player
            this.shadow = shadow
          }
        }
        break
      case MessageType.UPDATE_NTF:
        const elapsed = message.elapsed
        for (let i = 0, len = message.entities.length; i < len; i++) {
          const entity = message.entities[i]
          const shadow = this.shadows[entity.id]
          if (!shadow) continue;
          const {src, dst, offset} = entity
          shadow.checkpoints.push({elapsed, src, dst, offset})
          if (shadow.checkpoints.length > 128) {
            shadow.checkpoints = shadow.checkpoints.slice(-128)
          }
        }
        //console.log(message.entities)
        break
      default:
        
    }
  }
  
  joinRoom() {
    const {JoinRoomReq} = Protocols
    const room_id = 0
    const req = new JoinRoomReq({room_id})
    this.game.transport.send(MessageType.JOIN_ROOM_REQ, req)
  }
  
  update() {
    const lerp = Latency.MIN + Latency.MAX
    const now = this.getClientTime() - lerp
    
    this.debugInfo.text = `fps: ${this.game.time.fps}\nlatency: ${this.game.transport.latency}`
    for (let id in this.players) {
      const player = this.players[id]
      const {x, y} = this.edge2xy(player.src, player.dst, player.offset)
      player.x = x
      player.y = y
    }
    for (let id in this.shadows) {
      const shadow = this.shadows[id]
      const checkpoints = shadow.checkpoints
      
      const fromIndex = checkpoints.findIndex((checkpoint) => {
        return checkpoint.elapsed > now
      }) - 1
      if (fromIndex < 0) {
        continue
      }
      const from = checkpoints[fromIndex]
      const to = checkpoints[fromIndex + 1]
      const fromPos = this.edge2xy(from.src, from.dst, from.offset)
      const toPos = this.edge2xy(to.src, to.dst, to.offset)
      const k = (now - from.elapsed) / (to.elapsed - from.elapsed)
      const x = fromPos.x + (toPos.x - fromPos.x) * k
      const y = fromPos.y + (toPos.y - fromPos.y) * k
      
      shadow.x = x
      shadow.y = y
      shadow.checkpoints = checkpoints.slice(fromIndex)
    }
  }
  
  render() {
    const player = this.player
    const cursors = this.cursors
    if (!player) return
    
    if (cursors.left.isDown) {
      player.move(Direction.LEFT)
    } else if (cursors.right.isDown) {
      player.move(Direction.RIGHT)
    } else if (cursors.up.isDown) {
      player.move(Direction.UP)
    } else if (cursors.down.isDown) {
      player.move(Direction.DOWN)
    }
  }
}
