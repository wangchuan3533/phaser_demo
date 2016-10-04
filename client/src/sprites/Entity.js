import Phaser from 'phaser'

export default class extends Phaser.Sprite {

  constructor ({ game, asset, index, offset, id}) {
    super(game, 0, 0, asset)

    this.game = game
    this.id = id
    this.index = index
    this.offset = offset
    this.anchor.setTo(0.5)
    this.checkpoints = []
  }
}
