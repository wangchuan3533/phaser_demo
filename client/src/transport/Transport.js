import {Latency} from '../const'
import {MessageType, Message, decode} from '../protocol'
import WSTransport from './WSTransport'
import UDPTransport from './UDPTransport'

export default class Transport {
  constructor() {
    this.cbs = {
      open: [],
      close: [],
      message: []
    }
    
    this.start_time = 0
    this.offset = 0
    this.latency = 0
    this.transport = UDPTransport
    //this.transport = WSTransport
    
    this.transport.on('open', this.opencb.bind(this))
    this.transport.on('close', this.closecb.bind(this))
    this.transport.on('message', this.messsagecb.bind(this))
  }
  
  connect() {
    this.transport.connect()
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
  
  messsagecb(data, from) {
    if (this.transport == WSTransport) data = data.data
    setTimeout(() => {
      const message = decode(data)
      for (let i = 0; i < this.cbs.message.length; i++) {
        this.cbs.message[i](message)
      }
    }, Latency.random())
  }
  
  register(type, cb) {
    this.cbs[type] && this.cbs[type].push(cb)
  }
  
  send(msg) {
    setTimeout(() => {
      this.transport.send(new Buffer(msg.toArrayBuffer()))
    }, Latency.random())
  }
}
