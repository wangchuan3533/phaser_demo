import {Latency} from '../const'
export default class WSTransport {
  constructor() {
    this.cbs = {
      open: [],
      close: [],
      message: []
    }
  }
  
  connect() {
    const ws = new WebSocket('ws://127.0.0.1:8888/ws')
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
    for (let i = 0; i < this.cbs.message.length; i++) {
      this.cbs.message[i](evt)
    }
  }
  
  register(type, cb) {
    this.cbs[type] && this.cbs[type].push(cb)
  }
  
  send(data) {
    setTimeout(() => this.ws.send(data), Latency.random())
  }
}
