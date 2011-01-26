--[[
  Message type definitions
  Kevin Lynx
  1.26.2011
  Prototype document:
  .When get online, broadcast 0 and 600001H(with nickname and group name) messages.
  .Others will send 600003H(same as 600001) response to you.
  .If you receive 600001H, send 600003H to the sender.
  .Multicast 2000C9H to become a memeber of a group and later you can talk in the 
   group free.
  .Send private message process:
  	1.ask dest for encrypt public key, 72H
	2.dest answer public key, 73H
	3.src encrypt message by public key, send to dest, 400120H
	4.dest send response 21H
--]]
MSG_NOOP = 0x00
MSG_BR_ENTRY = 0x600001
MSG_BR_ENTRYANS = 0x600003
MSG_SEND_GROUP = 0x400023
MSG_BR_GROUPENTRY = 0x2000C9
