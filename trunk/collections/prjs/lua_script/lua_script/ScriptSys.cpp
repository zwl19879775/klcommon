///
///
///
#include "luainc.h"
#include "ScriptSys.h"
#include "Script.h"
#include "utils/file_loader.h"
#include "BindFuncs.h"

#define SCRIPT_NUM (4)
#define NOTIFY_ERR( ret, L ) if( ret == LUA_ERRRUN ) PrintError( L )

static void PrintError( lua_State *L )
{
	const char *s = lua_tostring( L, -1 );
	fprintf( stderr, "Lua pcall error : %s\n", s );	
}

bool ScriptSys::Init()
{
	m_IDIndex = 0;
	m_MainState = lua_open();
	luaL_openlibs( m_MainState );
	m_Holder = new ScriptHolder( m_MainState );
	m_Holder->Init();
	
	BuildFuncs();

	// test, add some scripts first.
	char file[512];
	for( int i = 0; i < SCRIPT_NUM; ++ i )
	{
		sprintf( file, "scripts\\%d.lua", i + 1 );
		FileLoader loader;
		loader.Load( file, FileLoader::BINARY );
		FileLoader::RawData data = loader.GetRawData();
		m_Holder->Add( file, (char*) data.buf, data.size );
	}

	return true;
}

void ScriptSys::Release()
{
	delete m_Holder;
	Script::clear();
	lua_close( m_MainState );
	m_Scripts.clear();
}

long ScriptSys::GenID()
{
	return ++m_IDIndex;
}

int ScriptSys::RunScript( const char *file )
{
	Script *s = NewScript();
	int ret = s->RunScript( file );
	if( ret != LUA_YIELD )
	{
		DelScript( s );
	}
	NOTIFY_ERR( ret, m_MainState );
	return ret;
}

int ScriptSys::RunStr( const char *str )
{
	Script *s = NewScript();
	int ret = s->RunStr( str );
	if( ret != LUA_YIELD )
	{
		DelScript( s );
	}
	NOTIFY_ERR( ret, m_MainState );
	return ret;
}

int ScriptSys::ResumeScript( long id )
{
	Script *s = GetScript( id );
	if( s != NULL )
	{
		int ret = s->Resume();
		if( ret != LUA_YIELD )
		{
			DelScript( s );
		}
		return ret;
	}
	return -1;
}

int ScriptSys::ResumeScript( lua_State *L )
{
	Script *s = GetScript( L );
	if( s != NULL )
	{
		int ret = s->Resume();
		if( ret != LUA_YIELD )
		{
			DelScript( s );
		}
		return ret;
	}
	return -1;
}

Script *ScriptSys::NewScript()
{
	Script *s = Script::create();
	s->Init( GenID(), m_MainState );
	m_Scripts[s->ID()] = s;
	BuildID( s );
	return s;
}

void ScriptSys::DelScript( Script *s )
{
	m_Scripts.erase( s->ID() );
	Script::destroy( s );
}

Script *ScriptSys::GetScript( long id )
{
	ScriptTableT::iterator it = m_Scripts.find( id );
	if( it != m_Scripts.end() )
	{
		return it->second;
	}
	return NULL;
}

Script *ScriptSys::GetScript( lua_State *L )
{
	for( ScriptTableT::iterator it = m_Scripts.begin();
		it != m_Scripts.end(); ++ it )
	{
		if( it->second->QueryState() == L )
		{
			return it->second;
		}
	}
	return NULL;
}

#define SCRIPT_ID "ScriptID"

long ScriptSys::QueryID( lua_State *L )
{
	//lua_pushstring( L, SCRIPT_ID );
	//lua_gettable( L, LUA_GLOBALSINDEX );
	//return (long) lua_tointeger( L, -1 );
	for( ScriptTableT::const_iterator it = m_Scripts.begin();
		it != m_Scripts.end(); ++ it )
	{
		if( it->second->QueryState() == L )
		{
			return it->second->ID();
		}
	}
	return 0;
}

void ScriptSys::BuildID( Script *s )
{
	/*
	lua thread shares the same global table, so these codes 
	are invalid.
	*/
	//lua_State *L = s->QueryState();
	//lua_pushstring( L, SCRIPT_ID );
	//lua_pushinteger( L, s->ID() );
	//lua_settable( L, LUA_GLOBALSINDEX );
}

void ScriptSys::BuildFuncs()
{
	lua_register( m_MainState, "wait", wait );
	lua_register( m_MainState, "call", call );
}


