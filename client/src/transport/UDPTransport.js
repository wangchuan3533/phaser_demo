import dgram from 'dgram'
const sock = dgram.createSocket('udp4')
const message = Buffer.from('Some bytes')
const HOST = 'ec2-54-175-246-94.compute-1.amazonaws.com'
//const HOST = '10.0.4.113'
const PORT = 11111
//const HOST = 'localhost'

export default {
  connect: function() {
    //sock.bind(22222)
    sock.send('hello', PORT, HOST, (err) => {
      if (err) console.log(err)
    })
  },
  
  send: function(msg) {
    sock.send(msg, PORT, HOST, (err) => {
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
