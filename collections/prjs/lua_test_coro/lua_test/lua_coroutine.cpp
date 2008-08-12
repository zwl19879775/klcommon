///
/// @file lua_coroutine.cpp
/// @author Kevin Lynx
/// @date 8.12.2008
///

extern "C"
{
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
}

#include <windows.h>
#include <list>
#include <algorithm>

struct TimerHandler
{
	DWORD _start;
	DWORD _delay;
	typedef void (*handler_fn)( void *arg );
	handler_fn _fn;
};

typedef std::list<TimerHandler*> TimerHandlerList;
TimerHandlerList gTimerHandlers;

void onTimerOut( void *arg )
{
	lua_State *L = (lua_State*) arg;
	lua_resume( L, 0 );
}

extern "C"
{
	int sleep( lua_State *L )
	{
		int sec = (int)lua_tonumber( L, -1 );
	
		TimerHandler *h = (TimerHandler*) malloc( sizeof( TimerHandler ) );
		h->_start = GetTickCount();
		h->_delay = sec * 1000;
		h->_fn = onTimerOut;

		gTimerHandlers.push_back( h );

		return lua_yield( L, 0 );
	}

}

int do_script( lua_State *L, const char *script )
{
	lua_pushcfunction( L, sleep );
	lua_setglobal( L, "sleep" );

	int error = luaL_loadfile( L, script );
	if( error != 0 )
	{
		fprintf( stderr, "load script [%s] failed\n", script );
		return -1;
	}

	lua_resume( L, 0 );
	return 0;
}

int main()
{
	lua_State *L = lua_open();
	luaopen_base( L );
	
	do_script( L, "test_coro.lua" );

	/* for testing purpose only */
	while( gTimerHandlers.size() != 0 )
	{
		DWORD now = GetTickCount();
		for( TimerHandlerList::iterator it = gTimerHandlers.begin(); it != gTimerHandlers.end(); )
		{
			TimerHandler *h = *it;
			if( h->_start + h->_delay <= now )
			{
				h->_fn( L );
				free( h );
				it = gTimerHandlers.erase( it );
				continue;
			}

			++ it;
		}

		Sleep( 1 );
	}

	/* clear */
	struct deletor
	{
		void operator()( TimerHandler *h )
		{
			free( h );
		}
	};
	std::for_each( gTimerHandlers.begin(), gTimerHandlers.end(), deletor() );

	lua_close( L );
	return 0;
}
