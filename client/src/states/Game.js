import Phaser from 'phaser'
import Player from '../sprites/Player'
import Shadow from '../sprites/Shadow'
import {Direction, TILE_SIZE, Latency} from '../const'
import TileMap from '../map/TileMap'
import Graph from '../map/Graph'
import {Message, MessageType, Protocols, decode} from '../protocol'

export default class extends Phaser.State {
  init() {}
  preload() {}

  create() {
    const path = this.cache.getBinary('path')
    this.map = new TileMap(path)
    
    const edge = this.cache.getBinary('edge')
    this.graph = new Graph(edge)
    
    const border = 0
    const graphics = this.add.graphics(0, 0)
    
    // draw roads
    graphics.lineStyle(1, 0xffff00, 1)
    graphics.beginFill(0xafafaf, 1)
    
    for (let index in this.map.nodeIdMap) {
      const {x, y} = this.map.index2xy(index);
      graphics.drawRect(x * TILE_SIZE + border, y * TILE_SIZE + border, TILE_SIZE - 2 * border, TILE_SIZE - 2 * border)
    }
    
    graphics.endFill()
    
    const initEdge = this.graph.randomEdge()
    const {x, y} = this.map.index2xy(initEdge.src)
    const player = new Player({
      game: this.game,
      x: (x + 0.5) * TILE_SIZE,
      y: (y + 0.5) * TILE_SIZE,
      map: this.map,
      graph: this.graph,
      src: initEdge.src
    })
    
    this.game.add.existing(player)
    this.player = player
    
    const shadow = new Shadow({
      game: this.game,
      x: player.x,
      y: player.y,
    })
    this.game.add.existing(shadow)
    this.shadow = shadow
    
    // cursors
    this.cursors = this.game.input.keyboard.createCursorKeys()
    
    // join room
    this.joinRoom()
    
    // regiester protocol
    
    this.game.transport.register('message', this.onMessage.bind(this))
    
    // show fps
    this.game.time.advancedTiming = true
    const debugInfo = this.add.text(this.world.width - 128, 32, 'debug info', { font: '16px Arial', fill: '#eeeeee', align: 'center' })
    debugInfo.anchor.setTo(0.5, 0.5)
    this.debugInfo = debugInfo
  }
  
  onMessage(evt) {
    const message = decode(evt.data)
    switch (message.type) {
      case MessageType.JOIN_ROOM_RES:
        this.player.src = message.entity.src
        this.player.dst = message.entity.dst
        this.player.offset = message.entity.offset
        this.player.id = message.entity.id
        
        this.shadow.src = message.entity.src
        this.shadow.dst = message.entity.dst
        this.shadow.offset = message.entity.offset
        this.shadow.id = message.entity.id
        break
      case MessageType.UPDATE_NTF:
        for (let i = 0, len = message.entities.length; i < len; i++) {
          const entity = message.entities[i]
          const {x, y} = this.map.edge2xy(entity.src, entity.dst, entity.offset)
          
          if (entity.id == this.player.id) {
            setTimeout(() => {
              this.shadow.x = (x + 0.5) * TILE_SIZE
              this.shadow.y = (y + 0.5) * TILE_SIZE
              if (Math.abs(this.shadow.x - this.player.x) + Math.abs(this.shadow.y - this.player.y) > 5 * TILE_SIZE) {
                this.player.src = entity.src
                this.player.dst = entity.dst
                this.player.offset = entity.offset
                this.player.direction = entity.direction
                this.player.nextDirection = entity.next_direction
              }
            }, Latency.random())
          }
        }
        //console.log(message.entities)
        break
      default:
        
    }
  }
  
  joinRoom() {
    const {JoinRoomReq} = Protocols
    const req = new JoinRoomReq({room_id: 0})
    const message = new Message({type: MessageType.JOIN_ROOM_REQ, data: req.toArrayBuffer()})
    this.game.transport.send(message.toArrayBuffer())
  }
  
  update() {
    this.debugInfo.text = `fps: ${this.game.time.fps}`
  }
  
  render() {
    const player = this.player
    const cursors = this.cursors
    
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
