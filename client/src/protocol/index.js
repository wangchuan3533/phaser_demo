import protobuf from 'protobufjs'
import camelcase from 'uppercamelcase'

const builder = protobuf.loadProtoFile('./assets/proto/messages.proto')
const namespace = 'demo.protocol.'

export const MessageType = builder.build(namespace + 'MessageType')
export const Message = builder.build(namespace + 'Message')
export const Protocols = {}

for (let messageType in MessageType) {
  const Protocol = builder.build(namespace + camelcase(messageType))
  Protocols[MessageType[messageType]] = Protocol
  Protocols[camelcase(messageType)] = Protocol
}

export const decode = (data) => {
  const msg = Message.decode(data)
  return msg
}
