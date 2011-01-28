function check_group(groupname)
    local len = string.len(groupname)
    local i = 1
    while i <= len do
        if string.byte(groupname, i) == 0 then
            break
        end
        i = i + 1
    end
    if i == 1 then
        return "null"
    elseif i > len then
        return groupname
    end
    return string.sub(groupname, 0, i-1)
end

function dump_string(s)
    print(string.byte(s, 0, string.len(s)))
end

--str = "abc" .. string.char(0) .. "def"
str = "abc" .. string.char(0)
dump_string(str)
cstr = check_group(str)
dump_string(cstr)

