--[[
  Test lua_stringconv module
  Kevin Lynx
  1.31.2011
--]]
require('stringconv')

--stable = { 0xD6, 0xD0, 0xCE, 0xC4, 0xB2, 0xE2, 0xCA, 0xD4 }
stable = { 0x41, 0x42, 0x43 }

gbks = ""
for i, v in ipairs(stable) do
    gbks = gbks ..string.char(v)
end
print("gbk:", string.byte(gbks, 0, string.len(gbks)))
utf8s = stringconv.g2u(gbks)
print("utf8s:"..utf8s)

gbkb = stringconv.u2g(utf8s)
print("rets:", string.byte(gbkb, 0, string.len(gbkb)))
