import Phaser from 'phaser'
import WSTransport from '../transport/WSTransport'
import {decode} from '../protocol'

export default class extends Phaser.State {
  init () {
    this.connected = false
  }

  preload () {
    const transport = new WSTransport()
    transport.register('open', this.onOpen.bind(this))
    transport.register('close', this.onClose.bind(this))
    
    transport.connect()
    this.game.transport = transport
    
    this.load.image('blackhole', 'assets/images/blackhole.png')
    this.load.image('diamond', 'assets/images/diamond.png')
    this.load.image('hurricane', 'assets/images/hurricane.png')
    this.load.image('ice', 'assets/images/ice.png')
    this.load.image('star', 'assets/images/star.png')
    
    this.load.spritesheet('dude', 'assets/images/dude.png', 32, 48)
    this.load.spritesheet('baddie', 'assets/images/baddie.png', 32, 32)
    
    this.load.binary('path', 'assets/map/6.tmx.path.data')
    this.load.binary('edge', 'assets/map/6.tmx.edge.data')
  }
  
  render () {
    if (this.connected) {
      this.state.start('Game')
    }
  }

  onOpen() {
    this.connected = true
  }
  
  onClose() {
    this.connected = false
  }
}
