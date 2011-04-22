
local main_id = arg[1]
print("test task : " .. main_id)

local ok, err = pcall( function()
    a = nil
    print(string.format("%s", a))
end )

if ok then 
    task.post(main_id, '', 0)
else
    task.post(main_id, err or '?', -1)
end

