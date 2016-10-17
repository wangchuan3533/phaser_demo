export default {
  log: function(content) {
    fs.appendFile('/tmp/phaser_demo_client.log', JSON.stringify(content) + '\n');
  }
}
