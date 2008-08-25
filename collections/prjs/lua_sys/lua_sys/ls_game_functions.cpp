///
///
///
#include "stdafx.h"
#include "ls_game_functions.h"
#include "ls_system.h"
#include "base/kllua-binder.h"

#include "../public/tools.h"

extern "C"
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

/**
  import, to import a lua script into lua state.
*/
static void ls_import( const char *file )
{
	lua_sys::System::instance().import( file );
}

int lua_error_handler( lua_State *L )
{
	const char *error = lua_tostring( L, -1 );
	char error_desc[512];
	sprintf( error_desc, "lua kernel error : %s\n", error );
	PutLogInfo( error_desc );

	return 0;
}

#define BINDER_TYPE( id ) binder##id##_type

#define MAKE_DEF( proto, id ) \
	typedef kl_common::lua_binder< proto, id > BINDER_TYPE( id )

#define BIND_FUNC( proto, id, L, func, name ) \
	MAKE_DEF( proto, id ); \
	kl_common::lua_bind<BINDER_TYPE( id )>( L, BINDER_TYPE( id )::func_type( func ), name )

int luaopen_game( lua_State *L )
{
	// import
	BIND_FUNC( void ( const char* ), 1, L, ls_import, "import" );
	// PutLogInfo
	BIND_FUNC( void ( const char* ), 2, L, PutLogInfo, "PutLogInfo" );

	return 1;
}
