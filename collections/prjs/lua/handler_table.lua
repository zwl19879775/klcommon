--[[
  Handle a function table to handle messages.
  Kevin Lynx
  1.26.2011
--]]
g_msg_handlers = { }

function mh_arg(udp, ip, port, fullheader, feiqheader, body)
	local arg = {}
    arg.udp = udp
	arg.ip = ip
	arg.port = port
	arg.fullheader = fullheader
	arg.feiqheader = feiqheader
	arg.body = body
	return arg
end

function mh_register(t, func)
	g_msg_handlers[t] = func
	logd(string.format("register message handler %s.", tohex(t)))
end

function mh_dispatch(t, arg)
	t = tonumber(t)
    local cmd = msg_get_cmd(t)
	local h = g_msg_handlers[cmd]
	if h == nil then
        dump_message(arg)
		return
	end
	h(arg)
end

