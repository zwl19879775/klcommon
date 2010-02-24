local newgt = {}
--setmetatable( newgt, {__index=_G, __newindex=_G} )
setmetatable( newgt, {__index=_G} )
setfenv( 1, newgt )
print( "1.lua" )
_ENV = 1
--setmetatable( newgt, {__index=_G, __newindex=_G} )
--setfenv( 1, newgt )
setfenv( 1, _G )
_T = 11
print( _ENV )
print( newgt._ENV )
print( newgt._T )
print( _G._T )
wait( 5 );
print( "1.lua wait timeout" )
print( _ENV )
