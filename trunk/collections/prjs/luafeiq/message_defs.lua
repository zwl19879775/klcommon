--[[
  Message type definitions
  Kevin Lynx
  1.26.2011
  Prototype document:
  .When get online, broadcast 0 and 01H(with nickname and group name) messages.
  .Others will send 03H(same as 01H) response to you.
  .If you receive 01H, send 03H to the sender.
  .Multicast C9H to become a memeber of a group and later you can talk in the 
   group free.
  .Send private message process:
   When you get online, mark ENCRYPT option 0, and the message will not be encrypted,
   1.Send message(20H) to dest
   2.Dest send response(21H) with a message number in the message sent from src.
   3.Src will check the message number response from dest to be sure the message
     arrived.
--]]
MSG_NOOP = 0x00
MSG_BR_ENTRY = 0x01
MSG_BR_EXIT = 0x02
MSG_BR_ENTRYANS = 0x03
MSG_SEND_GROUP = 0x23
MSG_BR_GROUPENTRY = 0xC9
MSG_SEND_MSG = 0x20
MSG_RECV_MSG = 0x21

OPT_ABSENCE = 0x100
OPT_ENCRYPT = 0x400000
OPT_FILEATTACH = 0x200000

MASK_CMD = 0x000000ff
MASK_OPT = 0xffffff00

bit = require("bit")

function msg_get_cmd(t)
    return bit.band(t, MASK_CMD)
end

function msg_get_opt(t)
    return bit.band(t, MASK_OPT)
end

function combine(a, b)
    return bit.bor(a, b)
end

function tohex(t)
    return bit.tohex(t)
end

