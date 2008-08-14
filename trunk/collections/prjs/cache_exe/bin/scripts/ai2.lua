-- ai 2
--[[
function on_dead()
	print( "on_dead2" )
end
--]]
--dofile( "scripts/ai1.lua" )
--load( ai1, "scripts/ai1.lua" )
--pcall( ai1 )
import_cached( "scripts/ai1.lua" )
-- and now, we can overwrite on_dead function in ai1.lua.
function on_dead()
	print( "on_dead2" )
end

