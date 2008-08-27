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

void my_fn_t()
{
	printf( "my_fn_t\n" );
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

int lua_error_handler( lua_State *L )
{
	fprintf( stderr, "lua kernal error : %s\n", lua_tostring( L, -1 ) );
	return 0;
}

void dump_stack( lua_State *L )
{
	int top = lua_gettop( L );
	int i ;

	printf( "******************** stack ********************\n" );
	for( i = top; i >= 1; -- i )
	{
		int t = lua_type( L, i );
		switch( t )
		{
		case LUA_TSTRING:
			printf( "%s\n", lua_tostring( L, t ) );
			break;

		case LUA_TBOOLEAN:
			printf( lua_toboolean( L, t ) ? "true\n" : "false\n" );
			break;
		
		case LUA_TNUMBER:
			printf( "%g\n", lua_tonumber( L, t ) );
			break;

		default:
			printf( "%s\n", lua_typename( L, t ) );
		}
	}
	printf( "******************** end stack ****************\n" );
}

int main()
{
	lua_State *L = lua_open();
	/* set lua error handler */
	lua_atpanic( L, lua_error_handler );
	luaL_openlibs( L );

	/* bind functions to the specify table */
	lua_newtable( L );
	lua_pushvalue( L, -1 );
	lua_setglobal( L, "mylib" );
	int idx = -2;

	dump_stack( L );

	typedef kl_common::lua_binder< void (), 1 > none_param_fn;
	kl_common::lua_bind<none_param_fn>( L, none_param_fn::func_type( my_fn ), "fn", idx );

	dump_stack( L );

	typedef kl_common::lua_binder< void (), 2 > none_param_fn2;
	kl_common::lua_bind<none_param_fn2>( L, none_param_fn2::func_type( my_fn_t ), "fn_t", idx );

	typedef kl_common::lua_binder< int ( const char* ), 3 > one_param_fn;
	kl_common::lua_bind<one_param_fn>( L, one_param_fn::func_type( my_fn2 ), "fn2", idx );

	Game game;
	typedef kl_common::lua_binder< const char* ( int, int ), 4 > mem_fn;
	kl_common::lua_bind<mem_fn>( L, mem_fn::func_type( game, &Game::get_str ), "mem_fn", idx );	

	typedef kl_common::lua_binder< void ( int, int, int ), 5 > param3_fn;
	kl_common::lua_bind<param3_fn>( L, param3_fn::func_type( my_fn3 ), "fn3", idx );

	test_function( L );

	/* test call lua function */
	kl_common::lua_caller<void( int, const char* )> fn1( L, "fn_lua" );
	fn1( 11, "kevin" );
	
	kl_common::lua_caller<void( void )> fn2( L, "t.test" );
	fn2();
	
	const kl_common::lua_caller<void( int, const char* )> fn3 = fn1;
	fn3( 22, "lynx" );

	lua_close( L );
	return 0;
}
