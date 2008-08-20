///
/// @file test_klluabinder.cpp
/// 
///
#include "kllua-binder.h"
#include "kllua-caller.h"
#include <stdio.h>

void my_fn()
{
	printf( "my_fn\n" );
}

int my_fn2( const char *str )
{
	printf( "my_fn2 : %s\n", str );
	return 11;
}

void my_fn3( int , int, int )
{
	printf( "my_fn3\n" );
}

struct Game
{
	const char *get_str( int n1, int n2 )
	{
		static char buf[256];
		sprintf( buf, "STR : %d %d", n1, n2 );
		printf( "Game::get_str : %s\n", buf );
		return buf;
	}
};

void test_function( lua_State *L )
{
	/* load the script and compile it, then execute it */
	if( luaL_loadfile( L, "test.lua" ) != 0 || lua_pcall( L, 0, 0, 0 ) )
	{
		fprintf( stderr, "%s\n", lua_tostring( L, -1 ) );
		return ;
	}
}

#define TO_STRING( x ) TO_STRING1( x )
#define TO_STRING1( x ) #x

int main()
{
	lua_State *L = lua_open();
	luaL_openlibs( L );

	typedef kl_common::lua_binder< void () > none_param_fn;
	kl_common::lua_bind<none_param_fn>( L, none_param_fn::func_type( my_fn ), "fn" );

	typedef kl_common::lua_binder< int ( const char* ) > one_param_fn;
	kl_common::lua_bind<one_param_fn>( L, one_param_fn::func_type( my_fn2 ), "fn2" );

	Game game;
	typedef kl_common::lua_binder< const char* ( int, int ) > mem_fn;
	kl_common::lua_bind<mem_fn>( L, mem_fn::func_type( game, &Game::get_str ), "mem_fn" );	

	typedef kl_common::lua_binder< void ( int, int, int ) > param3_fn;
	kl_common::lua_bind<param3_fn>( L, param3_fn::func_type( my_fn3 ), "fn3" );

	test_function( L );

	/* test call lua function */
	kl_common::lua_caller<void( int, const char* )> fn1( L, "fn_lua" );
	fn1( 11, "kevin" );
	
	lua_close( L );
	return 0;
}
