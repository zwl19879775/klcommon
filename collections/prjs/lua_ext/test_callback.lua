-- test callback.lua
require("callback")

-- this function should be a function registered from c/c++
function register (fn_name, arg1, arg2)
    local r = {}
    r.name = fn_name
    r.arg1 = arg1
    r.arg2 = arg2
    return r
end

function call (r)
    local fn = _G[r.name]
    fn(r.arg1, r.arg2)
end

local r = register (callback.wrap(function (s1, s2) print(s1, s2) end),
                    "hello", "world")
call(r)

