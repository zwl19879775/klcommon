-- config.lua for test lua
width = 800
height = 600
test_table = { name = "kevin", age = 22 }

-- test function
function test_func( str, num )
	print( "lua script : name = " .. str .. " age = " .. tostring( num ) )
	return 1;
end

-- call the c function
cprint( "kevin lynx", 22 )

-- test user data
user_id = user_id_new()
print( "lua script : " .. type( user_id ) )
print( user_id )
user_id_set( user_id, "kevin", 22 )
name, age = user_id_get( user_id )
print( "name = " .. name .. " age = " .. age )
