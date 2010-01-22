
#include "ScriptHolder.h"
#include "luainc.h"

#define SCRIPT_TABLE "Scripts"

bool ScriptHolder::Init()
{
	lua_pushliteral( m_luaState, SCRIPT_TABLE );
	lua_newtable( m_luaState );
	lua_settable( m_luaState, LUA_REGISTRYINDEX );
	return true;
}

bool ScriptHolder::Add( const char *file, const char *buf, size_t size )
{
	if( !GetScriptTable( m_luaState ) )
	{
		return false;
	}
	int index = lua_gettop( m_luaState ); // [script table]
	lua_pushstring( m_luaState, file ); // [script table, string]
	int ret = luaL_loadbuffer( m_luaState, buf, size, file ); // [script table, string, chunk]
	if( ret != 0 )
	{
		return false;
	}
	lua_settable( m_luaState, index ); // Scripts[file] = file_chunk
	return true;
}

bool ScriptHolder::Get( const char *file )
{
	return Get( file, m_luaState );
}

bool ScriptHolder::Get( const char *file, lua_State *L )
{
	if( !GetScriptTable( L ) )
	{
		return false;
	}
	int index = lua_gettop( L );
	lua_pushstring( L, file );
	lua_gettable( L, index );
	if( lua_isnil( L, -1 ) )
	{
		return false;
	}
	return true;
}

bool ScriptHolder::GetScriptTable( lua_State *L )
{
	lua_pushliteral( L, SCRIPT_TABLE );
	lua_gettable( L, LUA_REGISTRYINDEX );
	if( lua_isnil( L, -1 ) )
	{
		return false;
	}
	return true;
}
