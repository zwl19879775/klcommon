
require("LuaXML")

local xfile = xml.load("../data/luafeiq.xml")
if xfile == nil then
    print("load file failed.")
end
local root = xfile:find("luafeiq")
print(root)

