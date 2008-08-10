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

template <typename _Tp, int N>
int array_size( _Tp (&)[N] )
{
	return N;
}

template <typename _Tp>
struct char_traits;

template <>
struct char_traits<char*>
{
	static void type()
	{
		printf( "const char*\n" );
	}
};

template <int N>
struct char_traits< char [N] >
{
	static void type()
	{
		printf( "const char[%d]\n", N );
	}
};

template <typename _Tp>
void traits( _Tp &t )
{
	char_traits<_Tp>::type();
}

int main()
{
	int a[11];
	int size = array_size( a );

	char s[20];
	char *p = s;
	
	traits( s );
	traits( p );

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