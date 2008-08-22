-- to test table based script system
import( "tscripts/base1.lua" )

ai1 = {}
ai1.self = function()
	print( "lua : this is ai1" )
end

ai1.on_attack = base1.on_attack
