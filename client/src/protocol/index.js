import protobuf from 'protobufjs'
import camelcase from 'uppercamelcase'

const builder = protobuf.loadProtoFile('/assets/protocol/messages.proto')

export const MessageType = builder.build('MessageType')
export const Direction = builder.build('Direction')
export const Message = builder.build('Message')
export const Protocols = {}

for (var messageType in MessageType) {
  const Protocol = builder.build(camelcase(messageType))
  Protocols[MessageType[messageType]] = Protocol
  Protocols[camelcase(messageType)] = Protocol
}

export const decode = (data) => {
  const msg = Message.decode(data)
  const Protocol = Protocols[msg.type]
  const subMsg = Protocol.decode(msg.data)
  subMsg.type = msg.type
  return subMsg
}
