--[[
  Handle messages.
  Kevin Lynx
  1.26.2011
--]]
function handle_recv_data(udp, data, ip, port)
	local fullheader, pos = msg_read_fullheader(data)
	local feiqheader = msg_read_feiqheader(fullheader)
	local body = msg_read_body(data, pos)
	local t = msg_read_type(fullheader)
	local arg = mh_arg(udp, ip, port, fullheader, feiqheader, body)
	mh_dispatch(t, arg)
end

function mh_handle_group(arg)
	-- as the group message is multicast, so we can received other groups
	-- message which we may donot care.
	if not GROUP_RECV_OTHERS then
		return
	end
	-- ignore self group messages
	local username = msg_read_username(arg.fullheader)
	local pcname = msg_read_pcname(arg.fullheader)
	if luafeiq_isself(username, pcname) then
		return
	end
	local mac = msg_read_mac(arg.feiqheader)
	local bodys = msg_read_bodysize(arg.feiqheader)
	local decrypt_body = msg_decrypt_body(arg.body, bodys, mac)
    logi("-------------------------Group Message------------------------------------------")
    logi(string.format("%s:%s", username, pcname))
    logi(decrypt_body)
    local groupnum = msg_read_groupnum(decrypt_body)
    groupnum = check_string(groupnum)
	luafeiq_handle_recv_groupmsg(arg, groupnum,
		msg_read_groupbody(decrypt_body))
end

function mh_handle_entry_group(arg)
	-- as the group message is multicast, so we can received other groups
	-- message which we may donot care.
	if not GROUP_RECV_OTHERS then
		return
	end
	local mac = msg_read_mac(arg.feiqheader)
	local bodys = msg_read_bodysize(arg.feiqheader)
	local decrypt_body = msg_decrypt_body(arg.body, bodys, mac)
	local groupnum = msg_read_groupnum(decrypt_body)
    groupnum = check_string(groupnum)
	logi("receive group entry message: " .. groupnum)
    logi(string.format("%s:%s", msg_read_username(arg.fullheader), msg_read_pcname(arg.fullheader)))
    logi(decrypt_body)
	-- create a new group message 
	if not group_exist(groupnum) then
        -- check whether this group has been focused, if so it has a name.
        local groupname = config_get_groupname(groupnum)
		local group = group_create(groupnum, groupname)
		group_insert(group)
	end
end

function mh_handle_noop(arg)
end

function mh_handle_entry(arg)
	local nick = msg_read_username(arg.fullheader)
	local group = ""
	local s, _ = skipto(arg.body, string.char(0), 0)
	if s ~= nil then
		nick = string.sub(arg.body, 0, s - 1)
		group = string.sub(arg.body, s + 1)
	end
	logd(string.format("receive entry (%s)-(%s)", nick, group))
	local user = user_create(arg.ip, arg.port, nick, group)
	user_add(user)
    -- send entry answer
    send_br_entryans(arg.udp, arg.ip, arg.port)
end

function mh_handle_entry_exit(arg)
	logi(string.format("receive entry exist (%s)", arg.ip))
	user_remove(arg.ip)
end

function mh_handle_entry_ans(arg)
	local nick = msg_read_username(arg.fullheader)
	local group = ""
	local s, _ = skipto(arg.body, string.char(0), 0)
	if s ~= nil then
		nick = string.sub(arg.body, 0, s - 1)
		group = string.sub(arg.body, s + 1)
	end
	logd(string.format("receive entry answer from:%s-%s", arg.ip, arg.port))
	local user = user_create(arg.ip, arg.port, nick, group)
	user_add(user)
end

function mh_handle_recv_msg(arg)
    logi("-------------------------Private Message----------------------------------------")
    logi(string.format("%s:%s", msg_read_username(arg.fullheader), msg_read_pcname(arg.fullheader)))
    logi(arg.body)
    -- send response
    local msgno = readin(arg.fullheader, ':', 1)
    logd(string.format("msg number: %d", msgno))
    send_recved_msg(arg.udp, arg.ip, arg.port, msgno)
    luafeiq_handle_recv_privatemsg(arg, arg.body)
end

-- when i send a message to someone, someone will response
function mh_handle_send_msg(arg)
    -- currently i just ignore this, because i donot care whether my message
    -- has been sent successfully.
    logi(string.format("response send from <%s><%d>", arg.ip, arg.port))
end

-- register handlers
mh_register(MSG_SEND_GROUP, mh_handle_group)
mh_register(MSG_NOOP, mh_handle_noop)
mh_register(MSG_BR_ENTRY, mh_handle_entry)
mh_register(MSG_BR_EXIT, mh_handle_entry_exit)
mh_register(MSG_BR_ENTRYANS, mh_handle_entry_ans)
mh_register(MSG_SEND_MSG, mh_handle_recv_msg)
mh_register(MSG_RECV_MSG, mh_handle_send_msg)
mh_register(MSG_BR_GROUPENTRY, mh_handle_entry_group)

