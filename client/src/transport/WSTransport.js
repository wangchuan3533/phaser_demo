import {Latency} from '../const'
import {MessageType, Message, decode} from '../protocol'

export default class WSTransport {
  constructor() {
    this.cbs = {
      open: [],
      close: [],
      message: []
    }
    
    this.start_time = 0
    this.offset = 0
    this.latency = 0
  }
  
  connect() {
    //const ws = new WebSocket('ws://192.168.31.210:8888/ws')
    //const ws = new WebSocket('ws://127.0.0.1:9002/ws')
    //const ws = new WebSocket('ws://192.168.31.210:9002/ws')
    const ws = new WebSocket('ws://monster-io-sin.tuanguwen.com:9002/ws')
    ws.binaryType = 'arraybuffer'
    ws.onopen = this.opencb.bind(this)
    ws.onclose = this.closecb.bind(this)
    ws.onmessage = this.messsagecb.bind(this)
    this.ws = ws
  }
  
  opencb(evt) {
    for (let i = 0; i < this.cbs.open.length; i++) {
      this.cbs.open[i](evt)
    }
  }
  
  closecb(evt) {
    for (let i = 0; i < this.cbs.close.length; i++) {
      this.cbs.close[i](evt)
    }
  }
  
  messsagecb(evt) {
    setTimeout(() => {
      const message = decode(evt.data)
      for (let i = 0; i < this.cbs.message.length; i++) {
        this.cbs.message[i](message)
      }
    }, Latency.random())
  }
  
  register(type, cb) {
    this.cbs[type] && this.cbs[type].push(cb)
  }
  
  send(type, msg) {
    setTimeout(() => {
      const data = msg.toArrayBuffer()
      const message = new Message({type, data})
      this.ws.send(message.toArrayBuffer())
    }, Latency.random())
  }
}
