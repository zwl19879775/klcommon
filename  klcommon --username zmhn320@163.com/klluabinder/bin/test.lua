-- test kl-lua-binder

a = mylib.fn()
--print( a )
a = mylib.fn2( "lua script" )
print( a )

-- test member function
a = mylib.mem_fn( 1, 2 )
print( a )

mylib.fn3( 1, 2, 3 )

function fn_lua( age, name )
	print( "lua : fn  : ", name, age )
end

t = {}
t.test = function()
	print( "lua : t.test" )
end
