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
	mh_dispatch(t, fullheader, feiqheader, body)
end

function mh_handle_group(feiqheader, fullheader, body)
	local mac = msg_read_mac(feiqheader)
	local bodys = msg_read_bodysize(feiqheader)
	local decrypt_body = msg_decrypt_body(body, bodys, mac)
    print("--------------------------------------------------------------------------------")
    print(string.format("%s:%s", msg_read_username(fullheader), msg_read_pcname(fullheader)))
    print(decrypt_body)
end

-- register handlers
mh_register(0x400023, mh_handle_group)
