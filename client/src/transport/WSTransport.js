import {Latency} from '../const'
import {MessageType, Message, decode} from '../protocol'

export default {
  connect: function() {
    //const ws = new WebSocket('ws://192.168.31.210:8888/ws')
    const ws = new WebSocket('ws://127.0.0.1:9002/ws')
    //const ws = new WebSocket('ws://123.57.55.85:9002/ws')
    //const ws = new WebSocket('ws://192.168.31.210:9002/ws')
    //const ws = new WebSocket('ws://monster-io-sin.tuanguwen.com:9002/ws')
    //const ws = new WebSocket('ws://ec2-54-175-246-94.compute-1.amazonaws.com:9002/ws')
    ws.binaryType = 'arraybuffer'
    ws.onopen = this.onopen
    ws.onclose = this.onclose
    ws.onmessage = this.onmessage
    this.ws = ws
  },
  
  send: function(message) {
    this.ws.send(message)
  },
  
  on: function(type, cb) {
    switch (type) {
    case 'open':
      this.onopen = cb
      break;
    case 'message':
      this.onmessage = cb
      break;
    case 'close':
      this.onclose = cb
      break;
    }
  }
}
