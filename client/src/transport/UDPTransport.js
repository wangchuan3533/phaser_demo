import dgram from 'dgram'
const sock = dgram.createSocket('udp4')
const message = Buffer.from('Some bytes')

export default {
  connect: function() {
    sock.send('hello', 11111, 'localhost', (err) => {
      if (err) console.log(err)
    })
  },
  
  send: function(msg) {
    sock.send(msg, 11111, 'localhost', (err) => {
      if (err) console.log(err)
    })
  },
  
  on: function(type, cb) {
    switch (type) {
    case 'open':
      sock.on('listening', cb);
      break;
    case 'message':
      sock.on('message', cb);
      break;
    case 'close':
      sock.on('error', cb);
      break;
    }
  }
}
