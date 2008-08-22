-- to test table based script system
base1 = {}
base1.self = function()
	print( "lua : this is base1" )
end

base1.on_attack = function()
	print( "lua : base1 on_attack" )
end

