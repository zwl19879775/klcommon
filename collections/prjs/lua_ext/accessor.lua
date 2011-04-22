--[[
accessor.lua
Wrap game object property access.
Kevin Lynx
4.21.2011
--]]
module ("accessor", package.seeall)

-- implement specific --
function get_attr(id, key)
    print("get value of "..key)
    return "val of "..key
end

function set_attr(id, key, value)
    print("set value of "..key.. " to "..value)
end
-- implement specific ends --

local function index_event (a, key)
    return get_attr(a.obj_id, key)
end

local function newindex_event (a, key, value)
    return set_attr(a.obj_id, key, value)
end

-- create a property accessor for an object.
function wrap (obj_id)
    local a = {}
    local mt = {}
    mt.__index = index_event
    mt.__newindex = newindex_event
    a.obj_id = obj_id
    -- get_default_attrs(a) to be much more effictive
    setmetatable(a, mt)
    return a
end

