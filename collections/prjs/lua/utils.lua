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

function check_string(str)
    local len = string.len(str)
    local i = 1
    while i <= len do
        if string.byte(str, i) == 0 then
            break
        end
        i = i + 1
    end
    if i == 1 then
        return "null"
    elseif i > len then
        return str 
    end
    return string.sub(str, 0, i-1)
end

function string_empty(str)
    return str == nil or str == "null" or str == "" or str == string.char(0)
end

