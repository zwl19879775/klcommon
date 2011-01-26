require("task")
local id = task.id()
local tsk, err = task.create("test_task.lua", {id})
if tsk < 0 then
    print("create task failed: " .. tsk)
else
    print("create task success: " .. tsk)
end
local msg, flags = task.receive(-1)
if flags then
    print(msg)
end

