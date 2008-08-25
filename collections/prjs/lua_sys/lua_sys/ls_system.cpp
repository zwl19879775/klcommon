///
///
///
#define _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include "ls_system.h"
#include "ls_script_holder.h"
#include "ls_game_functions.h"

extern "C"
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}


namespace lua_sys
{
	System::System() : _L( 0 ), _holder( 0 )
	{
	}

	System::~System()
	{
	}

	bool System::initialize( ScriptHolder *holder )
	{
		assert( holder != NULL );
		_holder = holder;

		_L = lua_open();
		// set the error handler
		lua_atpanic( _L, lua_error_handler );
		luaL_openlibs( _L );
		luaopen_game( _L );

		return true;
	}

	void System::release()
	{
		lua_close( _L );
	}

	System &System::instance()
	{
		static System sys;
		return sys;
	}

	void System::build()
	{
		assert( _holder != NULL && _L != NULL );

		ScriptHolder::FileList &fl = _holder->get_file_list();
		for( ScriptHolder::FileList::iterator it = fl.begin(); it != fl.end(); ++ it )
		{
			import( *it->second );
		}
	}

	void System::import( ScriptHolder::FileInfo &file )
	{
		if( !file._imported )
		{
			luaL_loadbuffer( _L, file._content, file._size, NULL );
			lua_pcall( _L, 0, 0, 0 );
			file._imported = true;
		}
	}

	void System::import( const std::string &script_name )
	{
		ScriptHolder::FileInfo *file = _holder->get_file( script_name );
		if( file != 0 )
		{
			import( *file );
		}
		else
		{
			char error[512];
			sprintf( error, "import non-exist file [%s]", script_name.c_str() );
			lua_pushstring( _L, error );
			lua_error( _L );
		}
	}
}