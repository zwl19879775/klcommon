--[[
  Handle a function table to handle messages.
  Kevin Lynx
  1.26.2011
--]]
g_msg_handlers = { }

function mh_register(t, func)
	g_msg_handlers[t] = func
	print("register handler " .. t)
end

function mh_dispatch(t, fullheader, feiqheader, body)
	t = t * 1 -- make t to be a number
	local h = g_msg_handlers[t]
	if h == nil then
        dump_message(fullheader, body)
		return
	end
	h(feiqheader, fullheader, body)
end

