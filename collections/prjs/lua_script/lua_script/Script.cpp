///
///
///
#include "Script.h"
#include "luainc.h"
#include "ScriptSys.h"

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
		// currently, i donot know the difference between lua_resume and lua_pcall,
		// but in some scripts contained some yield operation, lua_pcall will cause
		// a runtime error while lua_resume works fine.
		return lua_resume( m_luaState, 0 ); //lua_pcall( m_luaState, 0, 0, 0 );
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
