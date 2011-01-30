--[[
  Put udp stuff in a lua task
  Kevin Lynx
  1.26.2011
  1.28.2011 unused code, instead IUP GUI.
--]]
local main_id = arg[1]

GROUP_NUM = ""

function handle_set_group(udp, buf)
    GROUP_NUM = readin(buf, ':', 1)
    logi("set group number : " .. GROUP_NUM)
end

function handle_send_msg(udp, buf)
    if GROUP_NUM == "" then
        logw("group number has not been set")
        return
    end
    local text = readin(buf, ':', 1)
    logi(string.format("send group message <%s> to <%s>", text, GROUP_NUM))
    send_group_msg(udp, GROUP_NUM, text)
end

function handle_send_groupentry(udp)
    logi(string.format("send group <%s> entry mssage", GROUP_NUM))
    send_br_groupentry(udp, GROUP_NUM)
end

function handle_chat_msg(udp, buf)
    logd("handle chat command");
    local ip = readin(buf, ':', 1)
    local text = readin(buf, ':', 2)
    send_chat_msg(udp, ip, BIND_PORT, text)
end

function handle_cmd(buf, flags, udp)
    local t = readin(buf, ':', 0)
    if t == "set" then
        handle_set_group(udp, buf)
    elseif t == "send" then
        handle_send_msg(udp, buf)
    elseif t == "group" then
        handle_send_groupentry(udp)
	elseif t == "chat" then
		handle_chat_msg(udp, buf)
    end
end

function run_task()
    dofile("luafeiq.lua")
    local udp = luafeiq_init()
    while true do
        local buf, flags, rc = task.receive(10)
        if flags == 1 then
            break
        end
        if rc == 0 then -- received some ui command
            handle_cmd(buf, flags, udp)
        end
        udp_runonce(udp)
    end
    task.post(main_id, "", 1)
end

local ret, err = pcall(run_task)
if not ret then
    task.post(main_id, err, 2)
end

