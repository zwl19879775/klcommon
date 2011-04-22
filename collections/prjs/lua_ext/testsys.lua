--[[
testsys.lua
Used to test sys lua module.
Kevin Lynx
4.22.2011
]]--

require("sys")

-- check sys module interfaces
sys.traverse_table (sys, function (k, v) print(k, b) end)

-- test accessor
local wrap_obj = sys._accessor (100)
local loc = wrap_obj.loc 
wrap_obj.hp = 100 
