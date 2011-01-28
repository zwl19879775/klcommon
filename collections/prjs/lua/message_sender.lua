--[[
  Send messages.
  Kevin Lynx
  1.26.2011
--]]
--[[
NICKNAME = "PC128"
GROUPNAME = "ELF"
--]]
NICKNAME = "PC128"
GROUPNAME = "2048"

function get_nickname_group()
    return NICKNAME .. string.char(0) .. GROUPNAME .. string.char(0)
end

function send_br_entry(udp)
    local feiqh = msg_create_feiqheader(0)
    local fullh = msg_create_fullheader(feiqh, MSG_NOOP)
    local msg = msg_create(fullh, "")
    udp:sendto(msg, BROADCAST_ADDR, BIND_PORT)

    fullh = msg_create_fullheader(feiqh, combine(MSG_BR_ENTRY, OPT_ABSENCE))
    msg = msg_create(fullh, get_nickname_group())
    udp:sendto(msg, BROADCAST_ADDR, BIND_PORT)
    logi("send br entry message")
end

function send_br_entryans(udp, ip, port)
    local feiqh = msg_create_feiqheader(0)
    local fullh = msg_create_fullheader(feiqh, combine(MSG_BR_ENTRYANS, OPT_ABSENCE))
    local msg = msg_create(fullh, get_nickname_group())
    udp:sendto(msg, ip, port)
    logi("send br entry asnwer message")
end

function send_group_msg(udp, group_num, text)
    local body = msg_create_group_body(text, group_num, MAC_ADDRESS)
    local feiqh = msg_create_feiqheader(string.len(body))
    local fullh = msg_create_fullheader(feiqh, combine(MSG_SEND_GROUP, OPT_FILEATTACH))
    local msg = msg_create(fullh, body)
    udp:sendto(msg, MULTI_ADDR, BIND_PORT)
    logi("send group text message")
end

function send_br_groupentry(udp, group_num)
    local body = msg_create_group_body("", group_num, MAC_ADDRESS)
    local feiqh = msg_create_feiqheader(string.len(body))
    local fullh = msg_create_fullheader(feiqh, combine(MSG_BR_GROUPENTRY, OPT_FILEATTACH))
    local msg = msg_create(fullh, body)
    udp:sendto(msg, MULTI_ADDR, BIND_PORT)
    logi("send group entry message.")
end

function send_recved_msg(udp, ip, port, msgno)
    local feiqh = msg_create_feiqheader(0)
    local fullh = msg_create_fullheader(feiqh, MSG_RECV_MSG)
    local msg = msg_create(fullh, msgno)
    udp:sendto(msg, ip, port)
    logi(string.format("send recved msg response to:%s-%d", ip, port))
end

function send_chat_msg(udp, ip, port, text)
    local feiqh = msg_create_feiqheader(0)
    local fullh = msg_create_fullheader(feiqh, combine(MSG_SEND_MSG, OPT_ABSENCE))
    local msg = msg_create(fullh, text)
    udp:sendto(msg, ip, port)
    logi(string.format("send chat msg to:%s-%d", ip, port))
end

