import Phaser from 'phaser'
import Entity from './Entity'
const speed = 3

export default class Shadow extends Entity {
  constructor(opts) {
    opts.asset = opts.asset || 'dude'
    super(opts)
    
    this.origScale = 1 
    this.scale.setTo(this.origScale, this.origScale)
    this.alpha = 0.5
  }
  
}
