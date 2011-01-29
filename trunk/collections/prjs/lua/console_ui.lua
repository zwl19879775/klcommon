--[[
  Handle console UI stuff
  Kevin Lynx
  1.26.2011
  1.28.2011 unused code, instead IUP GUI.
--]]
require("task")

local id = task.id()
local tsk, err = task.create("udp_task.lua", {id})
if tsk < 0 then
    print("create udp task failed: " .. tsk)
else
    print("create udp task success: " .. tsk)
end
while true do
    io.stdout:write("\ncommand>")
    local cmd = io.stdin:read("*l")
    if cmd == "quit" then
        break
    else
        task.post(tsk, cmd, 0)
    end
    local msg, flag, rc = task.receive(1) -- test if there are some errors
    if rc == 0 and flag == 2 then
        print("task run error: " .. msg)
        break;
    end
end
task.post(tsk, "", 1)
task.receive(1000)

