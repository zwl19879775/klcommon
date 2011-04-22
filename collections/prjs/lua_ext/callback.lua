--[[
callback.lua
Implement a callback method to c/c++, that means we can pass a lua function to c++.
Kevin Lynx
4.21.2011
--]]
module ("callback", package.seeall)

local fn_id = 0

local function generate_func_id ()
    fn_id = fn_id + 1
    return fn_id
end

local function del_callback (name)
    _G[name] = nil
end

local function create_callback_table (fn, name)
    local t = {}
    t.callback = fn
    setmetatable (t, {__call = 
        function (func, ...)
            func.callback (...)
            del_callback (name)
        end })
    return t
end

-- the only interface here, to wrap a function.
function wrap (fn)
    local id = generate_func_id()
    local fn_s = "_callback_fn"..id
    _G[fn_s] = create_callback_table(fn, fn_s)
    return fn_s
end


