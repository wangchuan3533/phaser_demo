import Phaser from 'phaser'
import Player from '../sprites/Player'
import Shadow from '../sprites/Shadow'
import {Direction, TILE_SIZE, Latency, TICK} from '../const'
import TileMap from '../map/TileMap'
import {Message, MessageType, Protocols} from '../protocol'
import fs from 'fs'

export default class extends Phaser.State {
  init() {
    this.players = {}
    this.shadows = {}
  }
  preload() {}

  create() {
    // prepare
    const tile_map = this.cache.getBinary('tile_map')
    this.map = new TileMap(tile_map)
    
    // create roads
    this.createRoads()
    
    // cursors
    this.cursors = this.game.input.keyboard.createCursorKeys()
    
    // sync time
    this.syncTime()
    setInterval(() => {
      this.syncTime()
    }, 1000)
    
    // join room
    this.joinRoom()
    
    // regiester protocol
    this.game.transport.register('message', this.onMessage.bind(this))
    
    // debug info
    this.game.time.advancedTiming = true
    const debugInfo = this.add.text(this.world.width - 128, 52, 'debug info', { font: '16px Arial', fill: '#eeeeee', align: 'center' })
    debugInfo.anchor.setTo(0.5, 0.5)
    this.debugInfo = debugInfo
  }
  
  getClientTime() {
    return Date.now() + this.game.transport.offset - this.game.transport.start_time
  }
  
  createRoads() {
    const border = 0
    const graphics = this.add.graphics(0, 0)
    
    // draw roads
    graphics.lineStyle(2, 0xa0a000, 0.5)
    graphics.beginFill(0xafafaf, 1)
    
    for (let i = 0; i < this.map.n_edges; i++) {
      const edge = this.map.getEdge(i);
      const src = this.map.getVertex(edge.src)
      const dst = this.map.getVertex(edge.dst)
      graphics.moveTo(src.x * TILE_SIZE, src.y * TILE_SIZE)
      graphics.lineTo(dst.x * TILE_SIZE, dst.y * TILE_SIZE)
      if (edge.direction == Direction.DIRECTION_RIGHT || edge.direction == Direction.DIRECTION_DOWN) {
        this.add.text((src.x + dst.x) / 2 * TILE_SIZE, (src.y + dst.y) / 2 * TILE_SIZE, i, {font: '10px', fill: '#eeeeee'})
      }
    }
    
    graphics.endFill()
    this.walls = graphics
  }
  
  createPlayer(opts) {
    const player = new Player({
      ...opts,
      game: this.game,
      map: this.map
    })
    
    this.game.add.existing(player)
    this.players[opts.id] = player
    return player
  }
  
  createShadow(opts) {
    const shadow = new Shadow({
      ...opts,
      game: this.game,
      map: this.map
    })
    
    this.game.add.existing(shadow)
    this.shadows[opts.id] = shadow
    return shadow
  }
  
  onMessage(message) {
    switch (message.type) {
      case MessageType.TIME_SYNC_RES:
        message = message.time_sync_res
        const client_recv_time = Date.now()
        const offset = ((message.server_recv_time - message.client_send_time) + (message.server_send_time - client_recv_time)) / 2
        const round_trip_time = (client_recv_time - message.client_send_time) - (message.server_send_time - message.server_recv_time)
        fs.appendFile('/tmp/hello', JSON.stringify({client_recv_time, offset, round_trip_time}) + '\n')
        this.game.transport.offset = offset
        this.game.transport.latency = round_trip_time
        break;
      case MessageType.JOIN_ROOM_RES:
        message = message.join_room_res
        this.game.transport.start_time = message.start_time
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
        message = message.update_ntf
        const elapsed = message.elapsed
        for (let i = 0, len = message.entities.length; i < len; i++) {
          const entity = message.entities[i]
          const shadow = this.shadows[entity.id]
          if (!shadow) continue;
          
          const {index, offset} = entity
          shadow.checkpoints.push({elapsed, index, offset})
          if (shadow.checkpoints.length > 128) {
            shadow.checkpoints = shadow.checkpoints.slice(-128)
          }
        }
        //console.log(message.entities)
        break
      case MessageType.ACTION_RES:
        message = message.action_res
        this.player.action_res(message)
        break
      default:
        
    }
  }
  
  joinRoom() {
    const {JoinRoomReq} = Protocols
    const room_id = 0
    const join_room_req = new JoinRoomReq({room_id})
    const type = MessageType.JOIN_ROOM_REQ
    const message = new Message({type, join_room_req})
    this.game.transport.send(message)
  }
  
  syncTime() {
    const {TimeSyncReq} = Protocols
    const client_send_time = Date.now()
    const time_sync_req = new TimeSyncReq({client_send_time})
    const type = MessageType.TIME_SYNC_REQ
    const message = new Message({type, time_sync_req})
    this.game.transport.send(message)
  }
  
  update() {
    const lerp = 40 + this.game.transport.latency * 2
    const now = this.getClientTime() - lerp
    
    this.debugInfo.text = `fps: ${this.game.time.fps}\nlatency: ${this.game.transport.latency}\noffset: ${this.game.transport.offset}`
    
    for (let id in this.players) {
      const player = this.players[id]
      const {x, y} = this.map.pos_to_xy(player.index, player.offset)
      player.x = x * TILE_SIZE
      player.y = y * TILE_SIZE
    }
    
    for (let id in this.shadows) {
      const shadow = this.shadows[id]
      const checkpoints = shadow.checkpoints
      const fromIndex = checkpoints.findIndex((checkpoint) => {
        return checkpoint.elapsed > now
      }) - 1
      if (fromIndex < 0) {
        console.log('no from')
        continue
      }
      const from = checkpoints[fromIndex]
      const to = checkpoints[fromIndex + 1]
      if (!to) {
        console.log('no to')
        continue
      }
      const fromPos = this.map.pos_to_xy(from.index, from.offset)
      const toPos = this.map.pos_to_xy(to.index, to.offset)
      const k = (now - from.elapsed) / (to.elapsed - from.elapsed)
      const x = fromPos.x + (toPos.x - fromPos.x) * k
      const y = fromPos.y + (toPos.y - fromPos.y) * k
      shadow.x = x * TILE_SIZE
      shadow.y = y * TILE_SIZE
      //console.log({fromIndex, length: checkpoints.length})
      shadow.checkpoints = checkpoints.slice(fromIndex)
    }
  }
  
  render() {
    const player = this.player
    const cursors = this.cursors
    if (!player) return
    
    if (cursors.left.isDown) {
      player.move(Direction.DIRECTION_LEFT)
    } else if (cursors.right.isDown) {
      player.move(Direction.DIRECTION_RIGHT)
    } else if (cursors.up.isDown) {
      player.move(Direction.DIRECTION_UP)
    } else if (cursors.down.isDown) {
      player.move(Direction.DIRECTION_DOWN)
    }
  }
}
