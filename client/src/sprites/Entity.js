import Phaser from 'phaser'

export default class extends Phaser.Sprite {

  constructor ({ game, asset, src, dst, offset, id}) {
    super(game, 0, 0, asset)

    this.game = game
    this.id = id
    this.src = src
    this.dst = dst
    this.offset = offset
    this.anchor.setTo(0.5)
    this.checkpoints = []
  }
}
