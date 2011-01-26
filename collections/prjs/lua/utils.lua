--[[
  Utility
  Kevin Lynx
  1.26.2011
--]]
function dump_message(arg)
    local t = msg_read_type(arg.fullheader)
    local fp = io.open(string.format("pkg_%x.dat", t), "ab+")
    fp:write(arg.fullheader, arg.body)
    fp:flush()
    fp:close()
end

