-- test kl-lua-binder

a = fn()
--print( a )
a = fn2( "lua script" )
print( a )

-- test member function
a = mem_fn( 1, 2 )
print( a )

fn3( 1, 2, 3 )
