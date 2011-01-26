--[[
  The main entry
  Kevin Lynx
  1.26.2011
--]]
print("luafeiq start...")

dofile("log.lua")
dofile("utils.lua")
dofile("userlist.lua")
dofile("message_defs.lua")
dofile("message.lua")
dofile("handler_table.lua")
dofile("message_handlers.lua")
dofile("udpentry.lua")
dofile("message_sender.lua")

TESTFLAG = false

function luafeiq_init()
	logi("luafeiq init...")
    local udp = udp_init()
    if TESTFLAG ~= true then
        send_br_entry(udp)
    end
    return udp
end

function luafeiq_run(udp)
    while true do
        udp_runonce(udp)
    end
end

function test_send_group_msg(text, udp)
    local group_num = "F8916B47";
    print(string.format("send text (%s) to (%s)", text, group_num));
    send_group_msg(udp, group_num, text)
end

-- TEST purpose
function luafeiq_test()
    local udp = luafeiq_init()
    test_send_group_msg("abc", udp)
    luafeiq_run(udp)
end

local test_flag = arg[1]
if test_flag == "1" then
    TESTFLAG = true
    print("luafeiq test")
    luafeiq_test()
end

