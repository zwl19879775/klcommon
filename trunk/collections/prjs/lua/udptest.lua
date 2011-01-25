--[[
 Feiq adapter IM coded by Kevin Lynx 
 1.25.2011
--]]
g_msg_handlers = { }

function skipto(s, c, i)
   local s,_ = string.find(s, c, i) 
   return s
end

function readin(str, c, cnt)
    local s = 0
    while cnt > 0 do
        s = skipto(str, c, s)
		if s == nil then 
			return nil 
		end
		s = s + 1
		cnt = cnt - 1
    end
    local e = skipto(str, c, s)
    if e == nil then
        return string.sub(str, s)
    else
		e = e - 1
        return string.sub(str, s, e)
    end
end

function msg_read_feiqheader(fullheader)
	return readin(fullheader, ':', 0)
end

function msg_read_fullheader(msg)
	local c = 5
	local s = 0
	while c > 0 do
		c = c - 1
		s = skipto(msg, ':', s) + 1
	end
	return string.sub(msg, 0, s - 1), s
end

function msg_read_body(msg, pos)
	return string.sub(msg, pos)
end

function msg_read_mac(feiqheader)
	return readin(feiqheader, '#', 2)
end

function msg_read_bodysize(feiqheader)
	return readin(feiqheader, '#', 5)
end

function msg_read_type(fullheader)
	return readin(fullheader, ':', 4)
end

function msg_decrypt_body(body, bodys, mac)
	local bf = blowfish.create(mac, string.len(mac))
	local decrypt = blowfish.decrypt(bf, body, bodys)
	blowfish.destroy(bf)
	return decrypt
end

function handle_recv_data(data, ip, port)
	print("recv data from " .. ip)
	print(data)
	local fullheader, pos = msg_read_fullheader(data)
	print("full header: " .. fullheader)
	local feiqheader = msg_read_feiqheader(fullheader)
	print("feiq header: " .. feiqheader)
	local body = msg_read_body(data, pos)
	local t = msg_read_type(fullheader)
	mh_dispatch(t, fullheader, feiqheader, body)
end

function mh_handle_group(feiqheader, fullheader, body)
	print("recv group message")
	local mac = msg_read_mac(feiqheader)
	local bodys = msg_read_bodysize(feiqheader)
	print(string.format("body:%s, size:%d", body, bodys))
	print(string.format("mac:%s, size:%d ", mac, string.len(mac)))
	local decrypt_body = msg_decrypt_body(body, bodys, mac)
	print("decrypt body: " .. decrypt_body)
end

-------------------------------------------------------------------------------
function mh_register(t, func)
	g_msg_handlers[t] = func
	print("register handler " .. t)
end

function mh_dispatch(t, fullheader, feiqheader, body)
	t = t * 1
	local h = g_msg_handlers[t]
	if h == nil then
		print(string.format("unhandled message type <%d><%s><%s>", t, fullheader, body))
		return
	end
	h(feiqheader, fullheader, body)
end

function mh_registerall()
	mh_register(0x400023, mh_handle_group)
end
-------------------------------------------------------------------------------

require("blowfish")
local socket = require("socket")
local udp = socket.udp()
local ret, msg = udp:setsockname("*", 2425)
if ret == nil then
    print(msg)
else
    print("bind ok")
end

ret, msg = udp:setoption("ip-add-membership", 
    { multiaddr= "226.81.9.8", interface="*" } )
if ret == nil then
    print(msg)
    return
else
    print("setoption success")
end
mh_registerall()
while true do
    local data, ip, port = udp:receivefrom()
	handle_recv_data(data, ip, port)
end

