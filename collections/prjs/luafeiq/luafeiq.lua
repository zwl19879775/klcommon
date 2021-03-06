--[[
  The main entry
  Kevin Lynx
  1.26.2011
--]]

dofile("log.lua")
dofile("utils.lua")
dofile("userlist.lua")
dofile("grouplist.lua")
dofile("message_defs.lua")
dofile("message.lua")
dofile("handler_table.lua")
dofile("message_handlers.lua")
dofile("udpentry.lua")
dofile("message_sender.lua")
dofile("message_history.lua")
dofile("config_reader.lua")

TESTFLAG = false

-- global object, handle these global data.
luafeiq = {}

function luafeiq_initdata()
	luafeiq.udp = nil
	luafeiq.on_recv_privatemsg = nil
	luafeiq.on_recv_groupmsg = nil
end

function luafeiq_handle_recv_privatemsg(arg, text)
	local user = user_get(arg.ip)
	if user == nil then
		logw(string.format("recv private message from an unknown user(%s)",
		arg.ip))
		return
	end
	-- insert a chat log entry
	chatlog_insert(user.message, text, user.nickname)
	-- notify listener
	if luafeiq.on_recv_privatemsg ~= nil then
		luafeiq.on_recv_privatemsg(user, text)
	end
end

function luafeiq_handle_recv_groupmsg(arg, groupnum, text)
	local user = user_get(arg.ip)
	if user == nil then
		logw(string.format("recv group message from an unknown user(%s)",
		arg.ip))
		return
	end
	local group = group_get(groupnum)
	if group == nil then
		logi(string.format("recv nonexist group %s message, create a new one", groupnum));
        -- query if we focused on this group, if so the group will have a name
        local name = config_get_groupname(groupnum)
		group = group_create(groupnum, name)
		group_insert(group)
	end
	-- insert a chat log entry
	chatlog_insert(group.message, text, user.nickname)
	if luafeiq.on_recv_groupmsg ~= nil then
		luafeiq.on_recv_groupmsg(user, group, text)
	end
end

function luafeiq_send_groupentry(udp)
    config_iterate_group(function(number, name) 
        send_br_groupentry(udp, number)
        logi(string.format("send group entry message (%s,%s)", number, name))
    end)
end

function luafeiq_init()
	logi("luafeiq init...")
    if config_load() then
        logi("load config success")
    end
    luafeiq.udp = udp_init()
    if TESTFLAG ~= true then
        send_br_entry(luafeiq.udp)
		luafeiq_send_groupentry(luafeiq.udp)
    end
    return luafeiq.udp
end

function luafeiq_release()
    send_br_entryexit(luafeiq.udp)
	luafeiq.udp:close()
end

function luafeiq_run(udp)
    while true do
        udp_runonce(udp)
    end
end

function luafeiq_udp()
	return luafeiq.udp
end

function luafeiq_isself(username, pcname)
	return username == config_loginname() and pcname == config_pcname()
end

-- TEST purpose
function luafeiq_test()
    local udp = luafeiq_init()
    luafeiq_run(udp)
end

local test_flag = arg[1]
if test_flag == "1" then
    TESTFLAG = true
    print("luafeiq test")
    luafeiq_test()
end

