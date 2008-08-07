/**
  @brief lua_test.c
*/
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

/**
  try stack
*/
void test_stack( lua_State *L )
{
	/* push a number on the stack */
	lua_pushnumber( L, 12 );
	assert( lua_isnumber( L, -1 ) );
	/* lua_to* function only get the value but will not remove it */
	assert( lua_tonumber( L, -1 ) == 12 );	
	/* get the stack size */
	printf( "stack size : %d\n", lua_gettop( L ) );
}

/**
  test run commands.
*/
void test_run_command( lua_State *L )
{
	char buf[512];
	int error;

	while( fgets( buf, sizeof( buf ), stdin ) != 0 )
	{
		/* load a chunk of lua codes, this macro represents lua_load function */
		/* param : state, buffer, buffer size, chunk name */
		error = luaL_loadbuffer( L, buf, strlen( buf ), "line" ) || 
		/* execute the chunk that have been loaded before */
		/* param : state, argument count, return value count, error function */
		lua_pcall( L, 0, 0, 0 );

		if( error != 0 )
		{
			/* when error occures, lua will push error message into the stack, 
			and we can get the message by the stack index -1 */
			fprintf( stderr, "%s", lua_tostring( L, -1 ) );
			lua_pop( L, 1 );
		}
	}
}

/**
  test read global variable in lua script.
*/
void test_get_global( lua_State *L )
{
	/* load the script and compile it, then execute it */
	if( luaL_loadfile( L, "config.lua" ) != 0 || lua_pcall( L, 0, 0, 0 ) )
	{
		fprintf( stderr, "%s\n", lua_tostring( L, -1 ) );
		return ;
	}

	/* push variable value onto the stack */
	lua_getglobal( L, "width" );
	lua_getglobal( L, "height" );

	{
		/* get the value on the stack */
		int width = (int) lua_tonumber( L, -2 );
		int height = (int) lua_tonumber( L, -1 );

		printf( "width ; %d height : %d\n", width, height );

		/* i suppose it's better remove these values on the stack */
		lua_remove( L, -1 );
		lua_remove( L, -2 );
	}
}

int main()
{
	/* create a new state */
	lua_State *L = lua_open();
	/* open the library the lua script needs */
	luaopen_base( L );
	luaopen_table( L );
	luaopen_string( L );
	luaopen_math( L );
	
	/* it seems it's not support this function in lua5.1.3 */
	/*luaopen_io( L );*/
	
	test_stack( L );
	test_get_global( L );

	lua_close( L );
	return 0;
}
