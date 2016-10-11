import protobuf from 'protobufjs'
import camelcase from 'uppercamelcase'

const builder = protobuf.loadProtoFile('/assets/proto/messages.proto')

export const MessageType = builder.build('MessageType')
export const Message = builder.build('Message')
export const Protocols = {}

for (let messageType in MessageType) {
  const Protocol = builder.build(camelcase(messageType))
  Protocols[MessageType[messageType]] = Protocol
  Protocols[camelcase(messageType)] = Protocol
}

export const decode = (data) => {
  const msg = Message.decode(data)
  return msg
}
