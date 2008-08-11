///
/// @file test_klluabinder.cpp
/// 
///
#include "kllua-binder.h"
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

void test_function( lua_State *L )
{
	/* load the script and compile it, then execute it */
	if( luaL_loadfile( L, "test.lua" ) != 0 || lua_pcall( L, 0, 0, 0 ) )
	{
		fprintf( stderr, "%s\n", lua_tostring( L, -1 ) );
		return ;
	}
}

void test_traits()
{
	//kl_common::lua::param_traits<double>::get_param( 0, -1 );
	//kl_common::lua::param_traits<int>::get_param( 0, -1 );
	//kl_common::lua::param_traits<char>::get_param( 0, -1 );
	//kl_common::lua::param_traits<long>::get_param( 0, -1 );
	//kl_common::lua::param_traits<const char*>::get_param( 0, -1 );
	//kl_common::lua::param_traits<char*>::get_param( 0, -1 );

	int a = kl_common::lua::return_number_traits<void>::count ;
}

int main()
{
	test_traits();

	lua_State *L = lua_open();
	luaL_openlibs( L );

	typedef kl_common::lua_binder< void () > none_param_fn;
	kl_common::lua_bind<none_param_fn>( L, my_fn, "fn" );

	typedef kl_common::lua_binder< int ( const char* ) > one_param_fn;
	kl_common::lua_bind<one_param_fn>( L, my_fn2, "fn2" );

	test_function( L );
	lua_close( L );
	return 0;
}