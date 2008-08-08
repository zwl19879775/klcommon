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
  test script file.
*/
void test_script_file( lua_State *L )
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

	/* test get global variables */
	{
		/* get the value on the stack */
		int width = (int) lua_tonumber( L, -2 );
		int height = (int) lua_tonumber( L, -1 );

		printf( "width : %d height : %d\n", width, height );

		/* i suppose it's better remove these values on the stack */
		lua_remove( L, -1 );
		lua_remove( L, -2 );
	}

	/* test get global tables */
	{
		const char *name;
		int age;

		/* test get table element */
		lua_getglobal( L, "test_table" );
		/* push the key */
		lua_pushstring( L, "name" );
		/* and now, the table is at the index of -2 of the stack */
		lua_gettable( L, -2 );
		/* get the name field, when the value is removed from the stack, the string is invalid */
		name = lua_tostring( L, -1 );
		printf( "test_table[name] = %s\n", name );
		/* remove the name from the stack */
		lua_pop( L, 1 );
		
		/* another key */
		lua_pushstring( L, "age" );
		lua_gettable( L, -2 );
		age = (int) lua_tonumber( L, -1 );
		printf( "test_table[age] = %d\n", age );
		lua_pop( L, 1 );
	}

	/* test call script functions */
	/* to run a script function is easy : 
		1. push the script function onto the stack;
		2. push the arguments onto the stack;
		3. call lua_pcall to run the function;
		4. get the return value(s) from the stack. 
	*/
	{
		int ret;
		/* push the function onto the stack */
		lua_getglobal( L, "test_func" );
		/* push arguments onto the stack : from left to right */
		lua_pushstring( L, "i'm kevin" );
		lua_pushnumber( L, 22 );
		/* run it : two arguments and one result */
		lua_pcall( L, 2, 1, 0 );
		/* get the return value */
		ret = (int) lua_tonumber( L, -1 );
		printf( "test_func returned : %d\n", ret );
		lua_pop( L, 1 );
	}
}

/**
  test register c function in lua.
*/
void test_set_func( lua_State *L )
{
	/* test register c-function into the lua script */
	/* to register a c-function into the script should make these steps :
		1. push a c-function onto the stack ;
		2. set it a global variable(function) in lua.

	   A c-function can be registered into lua must be the prototype like :
		int (*cfunc)( lua_State * );
	*/
	{
		int script_func( lua_State * );
		lua_pushcfunction( L, script_func );
		lua_setglobal( L, "cprint" );
	}
}

/**
  a sample function to be registered in lua.
  its prototype in lua is : number func( string_arg, number_arg )
*/
int script_func( lua_State *L )
{
	/* get the number arg */
	int num = (int) lua_tonumber( L, -1 );
	/* get the string arg */
	const char *str = lua_tostring( L, -2 );
	
	printf( "c-function called by lua : name = %s age = %d\n", str, num );

	/* one return value */
	return 1;
}

/**
  user data type.
*/
struct user_id
{
	char name[64];
	int age;
};

/**
  create a new user data 
*/
int user_id_new( lua_State *L )
{
	/* call lua_newuserdata will create a user data type in lua, and the user data is already on the stack */
	/* this data is managed bu lua, so the gc system will manage the memory for you */
	lua_newuserdata( L, sizeof( struct user_id ) );
	return 1;
}

/**
  get user's name and id.
*/
int user_id_get( lua_State *L )
{
	/* get the user_id pointer */
	struct user_id *u = (struct user_id*) lua_touserdata( L, -1 );
	/* push the name onto the stack */
	lua_pushstring( L, u->name );
	/* push the id onto the stack */
	lua_pushnumber( L, u->age );
	/* two return values */
	return 2;
}

/**
  set user's name and id.
*/
int user_id_set( lua_State *L )
{
	const char *name;
	int age;

	/* get the user_id pointer */
	struct user_id *u = (struct user_id*) lua_touserdata( L, -3 );
	/* get the name parameter */
	name = lua_tostring( L, -2 );
	/* get the age parameter */
	age = (int) lua_tonumber( L, -1 );

	/* update the user_id */
	strcpy( u->name, name );
	u->age = age;
	/* no return value */
	return 0;
}

/**
  test user data, should register some functions.
*/
void test_user_data( lua_State *L )
{
	lua_pushcfunction( L, user_id_new );
	lua_setglobal( L, "user_id_new" );

	lua_pushcfunction( L, user_id_get );
	lua_setglobal( L, "user_id_get" );

	lua_pushcfunction( L, user_id_set );
	lua_setglobal( L, "user_id_set" );
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
	
	/* register something first */
	test_set_func( L );
	test_user_data( L );

	test_stack( L );
	test_script_file( L );

	lua_close( L );
	return 0;
}
