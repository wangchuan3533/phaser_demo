import {Latency} from '../const'
import {MessageType, Message, decode} from '../protocol'

export default class WSTransport {
  constructor() {
    this.cbs = {
      open: [],
      close: [],
      message: []
    }
    this.latency = 0
    this.start_time = 0
    this.start_time_count = 0
    this.sendTime = {}
  }
  
  connect() {
    //const ws = new WebSocket('ws://192.168.31.210:8888/ws')
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
    setTimeout(() => {
      const message = decode(evt.data)
      if (message.type == MessageType.CHECKPOINT_RES && this.sendTime[message.id] > 0) {
        const now = Date.now()
        const latency = now - this.sendTime[message.id]
        this.latency = this.latency || latency
        this.latency = Math.floor(this.latency + (latency - this.latency) / 100)
        this.start_time = now - message.elapsed - latency / 2
        this.start_time_count = 1
        delete this.sendTime[message.id]
      } else if (message.type == MessageType.JOIN_ROOM_RES && this.sendTime[0] > 0) {
        const now = Date.now()
        const latency = now - this.sendTime[0]
        this.latency = this.latency || latency
        this.latency = Math.floor(this.latency + (latency - this.latency) / 100)
        const start_time = now - message.elapsed - latency / 2
        this.start_time_count += 1
        this.start_time = this.start_time + (start_time - this.start_time) / this.start_time_count
        delete this.sendTime[0]
      }
      
      for (let i = 0; i < this.cbs.message.length; i++) {
        this.cbs.message[i](message)
      }
    }, Latency.random())
  }
  
  register(type, cb) {
    this.cbs[type] && this.cbs[type].push(cb)
  }
  
  send(type, msg) {
    if (type == MessageType.CHECKPOINT_REQ) {
      this.sendTime[msg.id] = Date.now()
    } else if (type == MessageType.JOIN_ROOM_REQ) {
      this.sendTime[0] = Date.now()
    }
    setTimeout(() => {
      const data = msg.toArrayBuffer()
      const message = new Message({type, data})
      this.ws.send(message.toArrayBuffer())
    }, Latency.random())
  }
}
