/**
  @brief cache multiple script files, and execute them later,or multiple times.
  @author Kevin Lynx
  @date 8.13.2008

*/
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
#include <string.h>
#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

#define NAME_LEN 256

/**
  scripts manager.
*/
struct cached_Script
{
	lua_State *L;
	char name[NAME_LEN];
};

struct cached_Script *gCS;

int cached_get_script( struct cached_Script *c, const char *name );

/**
  extend function for lua to import other script files.
*/
static int import( lua_State *L )
{
	/* get the chunk cached script name */
	const char *str = luaL_checkstring( L, -1 );
	/* get the cached script on the stack */
	if( cached_get_script( gCS, str ) != 0 )
	{
		fprintf( stderr, "script error : import an unknown cached script ; %s\n", str );
		return 0;
	}
	/* run it */
	lua_pcall( L, 0, 0, 0 );

	return 0;
}

void cached_init( struct cached_Script *c, const char *name )
{
	c->L = lua_open();
	luaL_openlibs( c->L );
	strcpy( c->name, name );

	/* create a table that holds the cache script table on the registry table */
	lua_newtable( c->L );
	lua_setfield( c->L, LUA_REGISTRYINDEX, c->name );

	/* register some functions */
	lua_register( c->L, "import_cached", import );
}

void cached_deinit( struct cached_Script *c )
{
	lua_close( c->L );
	c->name[0] = 0;
}

/**
  @param name the cached script name
*/
int cached_add_script( struct cached_Script *c, const char *buf, size_t size, const char *name )
{
	/* get the cached script table */
	lua_getfield( c->L, LUA_REGISTRYINDEX, c->name );
	/* get the cached script */
	lua_getfield( c->L, -1, name );
	
	if( lua_isnil( c->L, -1 ) )
	{
		/* the cached script does not exist */
		/* pop the nil value */
		lua_pop( c->L, 1 );
		/* load the script and compile it, if ok, lua will push it on the stack */
		if( luaL_loadbuffer( c->L, buf, size, name ) != 0 )
		{
			/* failed, the error message is on the stack of -1 index */
			/* remove the cached script table */
			lua_remove( c->L, -2 );
			return -1;
		}
		/* copy the compiled code */
		lua_pushvalue( c->L, -1 );
		/* store it in the cached script table */
		lua_setfield( c->L, -3, name );
	}

	/* and now, the stack is : -1 compiled function ; -2 cached script table */
	lua_remove( c->L, -2 );
	lua_remove( c->L, -1 );	
	return 0;
}

/**
  get a cached script and you can run it.
*/
int cached_get_script( struct cached_Script *c, const char *name )
{
	lua_getfield( c->L, LUA_REGISTRYINDEX, c->name );
	lua_getfield( c->L, -1, name );
	if( lua_isnil( c->L, -1 ) )
	{
		/* the cached script does not exist */
		lua_pop( c->L, 1 );
		return -1;
	}

	/* the cached script already exist on the top of the stack */
	return 0;
}


/**
  file info 
*/
struct file_info
{
	char *buf;
	size_t size;
};

int file_new( struct file_info *file, const char *file_name )
{
	FILE *fp = fopen( file_name, "rb" );
	if( fp == 0 )
	{
		return -1;
	}

	fseek( fp, 0, SEEK_END );
	file->size = (size_t)ftell( fp );
	fseek( fp, 0, SEEK_SET );

	file->buf = (char*) malloc( file->size );
	fread( file->buf, file->size, 1, fp );
	fclose( fp );

	return 0;
}

void file_free( struct file_info *file )
{
	file->size = 0;
	free( file->buf );
}

int cached_load( struct cached_Script *c, const char *cfg_file )
{
	FILE *fp = fopen( cfg_file, "r" );
	char line[256];
	struct file_info file;

	if( fp == 0 )
	{
		return -1;
	}

	while( !feof( fp ) )
	{
		fscanf( fp, "%s", line );
		/* load the file */
		if( file_new( &file, line ) != 0 )
		{
			fprintf( stderr, "warning : load script file [%s] failed\n", line );
			continue;
		}

		/* add the script to the cached script table */
		cached_add_script( c, file.buf, file.size, line );
		/* free */
		file_free( &file );
	}

	return 0;
}

int cached_call( struct cached_Script *c, const char *name )
{
	int ret = cached_get_script( c, name );
	if( ret != 0 )
	{
		fprintf( stderr, "failed to get cached script : %s\n", name );
		return -1;
	}

	/* run it from ai function , note : here is a trick : 
	    First : we run the whole script so that we can find some functions later;
		Second : find the functions which we want to call in the global table;
		Third : run the function we get 
	*/
	/* run the whole script */
	lua_pcall( c->L, 0, 0, 0 );
	/* and now, the functions in the script will be put in the global table , we can get it */
	lua_getglobal( c->L, "ai" );
	/* push the argument */
	lua_pushstring( c->L, name );
	/* and run the function */
	return lua_pcall( c->L, 1, 0, 0 );
}

int main()
{
	int ret;
	struct cached_Script c;
	char name[256];

	cached_init( &c, "cached_script" );
	ret = cached_load( &c, "config.ini" );
	if( ret != 0 )
	{
		fprintf( stderr, "load 'config.ini' failed\n" );
		exit( -1 );
	}
	gCS = &c;

	printf( "load all scripts in memory, and cached them ok\n" );
	printf( "input any script name you configed and run it\n" );

	while( scanf( "%s", name ) )
	{
		printf( "executing [%s]\n\n", name );
		
		cached_call( &c, name );

		printf( "\nexecute [%s] finished\n\n", name );
	}

	cached_deinit( &c );
	return 0;
}