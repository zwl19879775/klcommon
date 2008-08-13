-- ai 2
--[[
function on_dead()
	print( "on_dead2" )
end
--]]
loadfile( "ai1.lua" )

-- and now, we can overwrite on_dead function in ai1.lua.
function on_dead()
	print( "on_dead2" )
end

ai()

