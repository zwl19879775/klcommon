--[[
  Handle messages.
  Kevin Lynx
  1.26.2011
--]]
function handle_recv_data(data, ip, port)
	local fullheader, pos = msg_read_fullheader(data)
	local feiqheader = msg_read_feiqheader(fullheader)
	local body = msg_read_body(data, pos)
	local t = msg_read_type(fullheader)
	local arg = mh_arg(ip, port, fullheader, feiqheader, body)
	mh_dispatch(t, arg)
end

function mh_handle_group(arg)
	local mac = msg_read_mac(arg.feiqheader)
	local bodys = msg_read_bodysize(arg.feiqheader)
	local decrypt_body = msg_decrypt_body(arg.body, bodys, mac)
    logi("--------------------------------------------------------------------------------")
    logi(string.format("%s:%s", msg_read_username(arg.fullheader), msg_read_pcname(arg.fullheader)))
    logi(decrypt_body)
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
end

function mh_handle_entry_ans(arg)
	logd(string.format("receive entry answer from:%s-%s", arg.ip, arg.port))
	mh_handle_entry(arg) -- the same as mh_handle_entry
end

-- register handlers
mh_register(MSG_SEND_GROUP, mh_handle_group)
mh_register(MSG_NOOP, mh_handle_noop)
mh_register(MSG_BR_ENTRY, mh_handle_entry)
mh_register(MSG_BR_ENTRYANS, mh_handle_entry_ans)

