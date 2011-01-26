--[[
  Send messages.
  Kevin Lynx
  1.26.2011
--]]

NICKNAME = "PC128"
GROUPNAME = "ELF"

function get_nickname_group()
    return NICKNAME .. string.char(0) .. GROUPNAME .. string.char(0)
end

function send_br_entry(udp)
    local feiqh = msg_create_feiqheader(0)
    local fullh = msg_create_fullheader(feiqh, MSG_NOOP)
    local msg = msg_create(fullh, "")
    udp:sendto(msg, BROADCAST_ADDR, BIND_PORT)

    fullh = msg_create_fullheader(feiqh, MSG_BR_ENTRY)
    msg = msg_create(fullh, get_nickname_group())
    udp:sendto(msg, BROADCAST_ADDR, BIND_PORT)
    logi("send br entry message")
end

function send_group_msg(udp, group_num, text)
    local body = msg_create_group_body(text, group_num, MAC_ADDRESS)
    local feiqh = msg_create_feiqheader(string.len(body))
    local fullh = msg_create_fullheader(feiqh, MSG_SEND_GROUP)
    local msg = msg_create(fullh, body)
    udp:sendto(msg, MULTI_ADDR, BIND_PORT)
    logi("send group text message")
end

function send_br_groupentry(udp, group_num)
    local body = msg_create_group_body("", group_num, MAC_ADDRESS)
    local feiqh = msg_create_feiqheader(string.len(body))
    local fullh = msg_create_fullheader(feiqh, MSG_BR_GROUPENTRY)
    local msg = msg_create(fullh, body)
    udp:sendto(msg, MULTI_ADDR, BIND_PORT)
    logi("send group entry message.")
end
