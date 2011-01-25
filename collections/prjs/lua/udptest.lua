function skipto(s, c, i)
   local s,_ = string.find(s, c, i) 
   return s
end

function readin(str, c, c)
    local s = 0
    while c > 0 and s != nil do
        s = skipto(str, c, s)
    end
    if s == nil then
        return nil
    end
    local e = skipto(str, c, s)
    if e == nil then
        return string.sub(str, s)
    else
        return string.sub(str, s, e)
    end
end

local socket = require("socket")
local udp = socket.udp()
local ret, msg = udp:setsockname("*", 2425)
if ret == nil then
    print(msg)
else
    print("bind ok")
end

ret, msg = udp:setoption("ip-add-membership", 
    { multiaddr= "226.81.9.8", interface="*" } )
if ret == nil then
    print(msg)
    return
else
    print("setoption success")
end
while true do
    local data, ip, port = udp:receivefrom()
    print("recv data:"..data)
end

