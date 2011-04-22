-- test_accessor.lua
require ("accessor")

function get_sel_obj()
    return 1
end

local obj_id = get_sel_obj()
local wrap_obj = accessor.wrap (obj_id)
local loc = wrap_obj.loc -- GetAttr(player_id, "loc")
wrap_obj.hp = 100 --SetAttr(player_id, "hp", 100)

