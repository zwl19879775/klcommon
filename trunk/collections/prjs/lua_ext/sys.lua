--[[
sys.lua is a collection of some util functions to extend game script module.
Including:

* pass local function as function call arguments to c/c++
* limit global lua symbol create process
* wrap an convient accessor for our game object properties

* Kevin Lynx
* 4.22.2011
]]--

module ("sys", package.seeall)
require ("callback")
require ("accessor")

-- import callback.wrap 
_callback = callback.wrap
-- import accessor.wrap
_accessor = accessor.wrap

-- traverse a table, fn(k, v)
function traverse_table (t, fn)
    for k, v in pairs(t) do
        fn(k, v)
    end
end

