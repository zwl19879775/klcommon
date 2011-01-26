--[[
  The main entry
  Kevin Lynx
  1.26.2011
--]]
print("luafeiq start...")

dofile("utils.lua")
dofile("message.lua")
dofile("handler_table.lua")
dofile("message_handlers.lua")
dofile("udpentry.lua")
dofile("message_sender.lua")

UDP = nil
TESTFLAG = false

function luafeiq_init()
    local udp = udp_init()
    if TESTFLAG ~= true then
        send_br_entry(udp)
    end
    UDP = udp
    return udp
end

function luafeiq_run()
    while true do
        udp_runonce(UDP)
    end
end

function test_send_group_msg(text, udp)
    local group_num = "F8916B47";
    print(string.format("send text (%s) to (%s)", text, group_num));
    send_group_msg(udp, group_num, text)
end

-- TEST purpose
function luafeiq_test()
    luafeiq_init()
    test_send_group_msg("abc", UDP)
    luafeiq_run()
end

local test_flag = arg[1]
if test_flag == "1" then
    TESTFLAG = true
    print("luafeiq test")
    luafeiq_test()
end

