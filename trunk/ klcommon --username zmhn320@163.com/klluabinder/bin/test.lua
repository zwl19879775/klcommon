-- test kl-lua-binder

a = fn()
--print( a )
a = fn2( "lua script" )
print( a )

-- test member function
a = mem_fn( 1, 2 )
print( a )

fn3( 1, 2, 3 )

function fn_lua( age, name )
	print( "lua : fn  : ", name, age )
end

t = {}
t.test = function()
	print( "lua : t.test" )
end
