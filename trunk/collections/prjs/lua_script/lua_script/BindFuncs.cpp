
#include "luainc.h"
#include "BindFuncs.h"
#include "TimerManager.h"
#include "ScriptSys.h"

static void TimeOut( void *arg )
{
	lua_State *L = (lua_State*) arg;
	ScriptSys::getSingleton().ResumeScript( L );
}

int wait( lua_State *L )
{
	// The lua state here is different from the main state, it's the thread created from the main state.
	long sec = (long)lua_tointeger( L, -1 );
	TimerManager::getSingleton().schedule( TIME_OUT_V( sec * 1000 ), 0, TimeOut, L );
	return lua_yield( L, 0 );
}

int call( lua_State *L )
{
	const char *file = lua_tostring( L, -1 );
	ScriptSys::getSingleton().RunScript( file );
	return 0;
}
