import dgram from 'dgram'
const sock_recv = dgram.createSocket('udp4')
const sock_send = dgram.createSocket('udp4')
const message = Buffer.from('Some bytes')

export default {
  connect: function() {
    sock_recv.bind(22000);
    sock_send.send('127.0.0.1 22000', 11111, 'localhost', (err) => {
      if (err) console.log(err)
    })
  },
  
  send: function(msg) {
    sock_send.send(msg, 11111, 'localhost', (err) => {
      if (err) console.log(err)
    })
  },
  
  on: function(type, cb) {
    switch (type) {
    case 'open':
      sock_recv.on('listening', cb);
      break;
    case 'message':
      sock_recv.on('message', cb);
      break;
    case 'close':
      sock_recv.on('error', cb);
      break;
    }
  }
}
