--[[
  Handle a function table to handle messages.
  Kevin Lynx
  1.26.2011
--]]
g_msg_handlers = { }

function mh_arg(ip, port, fullheader, feiqheader, body)
	local arg = {}
	arg.ip = ip
	arg.port = port
	arg.fullheader = fullheader
	arg.feiqheader = feiqheader
	arg.body = body
	return arg
end

function mh_register(t, func)
	g_msg_handlers[t] = func
	logd(string.format("register message handler %d.", t))
end

function mh_dispatch(t, arg)
	t = t * 1 -- make t to be a number
	local h = g_msg_handlers[t]
	if h == nil then
        dump_message(arg)
		return
	end
	h(arg)
end

