--[[
  Utility
  Kevin Lynx
  1.26.2011
--]]
function dump_message(fullheader, body)
    local t = msg_read_type(fullheader)
    local fp = io.open(string.format("pkg_%x.dat", t), "ab+")
    fp:write(fullheader, body)
    fp:flush()
    fp:close()
end

