-module(protocol).
-include("common.hrl").

-export([decode/1, encode/1]).

decode(Bin) ->
  Message = messages_pb:decode_message(Bin),
  case Message of
    #message{type = 'JOIN_ROOM_REQ', data = Data} ->
      messages_pb:decode_joinroomreq(Data);
    #message{type = 'JOIN_ROOM_RES', data = Data} ->
      messages_pb:decode_joinroomres(Data);
    #message{type = 'LEAVE_ROOM_REQ', data = Data} ->
      messages_pb:decode_leaveroomreq(Data);
    #message{type = 'LEAVE_ROOM_RES', data = Data} ->
      messages_pb:decode_leaveroomres(Data);
    #message{type = 'CHECKPOINT_REQ', data = Data} ->
      messages_pb:decode_checkpointreq(Data);
    #message{type = 'CHECKPOINT_RES', data = Data} ->
      messages_pb:decode_checkpointres(Data);
    #message{type = 'UPDATE_NTF', data = Data} ->
      messages_pb:decode_updatentf(Data);
    _Other ->
      error
  end.

encode(Msg) ->
  Encoded = messages_pb:encode(Msg),
  case Msg of
    #joinroomreq{} ->
      messages_pb:encode(#message{type = 'JOIN_ROOM_REQ', data = Encoded});
    #joinroomres{} ->
      messages_pb:encode(#message{type = 'JOIN_ROOM_RES', data = Encoded});
    #leaveroomreq{} ->
      messages_pb:encode(#message{type = 'LEAVE_ROOM_REQ', data = Encoded});
    #leaveroomres{} ->
      messages_pb:encode(#message{type = 'LEAVE_ROOM_RES', data = Encoded});
    #checkpointreq{} ->
      messages_pb:encode(#message{type = 'CHECKPOINT_REQ', data = Encoded});
    #checkpointres{} ->
      messages_pb:encode(#message{type = 'CHECKPOINT_RES', data = Encoded});
    #updatentf{} ->
      messages_pb:encode(#message{type = 'UPDATE_NTF', data = Encoded});
    _Other ->
      error
  end.
