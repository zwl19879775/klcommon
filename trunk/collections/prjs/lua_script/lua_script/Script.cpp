///
///
///
#include "Script.h"
#include "luainc.h"
#include "ScriptSys.h"
#include <assert.h>
#include "tolua++.h"

bool Script::Init( long id, lua_State *mainState )
{
	m_ID = id;
	m_luaState = lua_newthread( mainState );
	return true;
}

int Script::RunScript( const char *file )
{
	if( ScriptSys::getSingleton().GetHolder().Get( file, m_luaState ) )
	{
		// create some local variables in this state.
		int t = BeginLocalEnv( lua_gettop( m_luaState ) );
		assert( t == lua_gettop( m_luaState ) );
		// currently, i donot know the difference between lua_resume and lua_pcall,
		// but in some scripts contained some yield operation, lua_pcall will cause
		// a runtime error while lua_resume works fine.
		int ret= lua_resume( m_luaState, 0 ); //lua_pcall( m_luaState, 0, 0, 0 );
		EndLocalEnv();
		return ret;
	}
	return -1;	
}

int Script::RunStr( const char *str )
{
	return luaL_dostring( m_luaState, str );
}

int Script::Resume()
{
	return lua_resume( m_luaState, 0 );
}

int Script::BeginLocalEnv( int funcIndex )
{
	// create env table
	lua_newtable( m_luaState );
	int envT = lua_gettop( m_luaState );

	// create { __index=_G } table
	lua_newtable( m_luaState );
	int t = lua_gettop( m_luaState );
	lua_pushstring( m_luaState, "__index" );
	lua_getfield( m_luaState, LUA_GLOBALSINDEX, "_G" );
	lua_settable( m_luaState, t );
	
	// set the metatable for the env table
	lua_setmetatable( m_luaState, envT );
	
	// create some local variables in the env table
	lua_pushinteger( m_luaState, (lua_Integer)this );
	lua_setfield( m_luaState, envT, "_ENV" );
	tolua_pushusertype( m_luaState, this, "Script" );
	lua_setfield( m_luaState, envT, "_Script" );

	// copy the env table
	lua_pushvalue( m_luaState, envT );

	// set the env
	lua_setfenv( m_luaState, funcIndex );
	
	// reset "__newindex" metamethod, to write variable in _G table.
	{
		// create { __index=_G, __newindex=_G } table
		lua_newtable( m_luaState );
		int t = lua_gettop( m_luaState );
		lua_pushstring( m_luaState, "__index" );
		lua_getfield( m_luaState, LUA_GLOBALSINDEX, "_G" );
		lua_settable( m_luaState, t );
		lua_pushstring( m_luaState, "__newindex" );
		lua_getfield( m_luaState, LUA_GLOBALSINDEX, "_G" );
		lua_settable( m_luaState, t );

		// set the metatable for the env table
		lua_setmetatable( m_luaState, envT );

		// set the env
		lua_setfenv( m_luaState, funcIndex );
	}
	return funcIndex;
}

void Script::EndLocalEnv()
{
}
