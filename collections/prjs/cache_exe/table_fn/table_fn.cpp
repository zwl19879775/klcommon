/**
  lua_cache中，采用的方案是预先将所有的脚本内容编译，并作为匿名函数保存于一个表中，而这个表
  又被保存于LUA的注册表中。每一次要执行某个脚本时，就从这个表里获取出编译好的内容并执行之。
  这种方案的优势在于：脚本里可以有全局执行代码，代码不用分成函数块。
 
  如果涉及到导入其他脚本，则会在执行期间递归地执行导入的脚本，从而保证全局符号最新。

 table_fn方案：
 每一个脚本都单独对应着一个表，要求脚本以如下方式编写：
 --a1.lua
 a1 = {}
 a1.on_move = function()
 --...
 end
 a1.on_attack = function()
 --...
 end
 a1.index = 0

 如果a1的某个函数和其他表里的函数相同，则需要显示地表示这种关系，同时，需要在文件头导入那个表：

 import( "base1.lua" )

 a1.on_dead = base1.on_dead

 C++方面的支持：

 宿主程序先以file_name - file_content 的形式保存所有脚本文件。然后再lua_pcall所有脚本文件，使其
 符号可见。当遇到import时，就根据参数lua_pcall那个文件（如果那个文件没有被pcall的话）。

*/
extern "C"
{
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
}

#include <map>
#include <string>
#include <stdio.h>

struct file_info
{
	char *content;
	bool imported;
	size_t size;
};

typedef std::map<std::string, file_info> file_list_type;

file_list_type gFiles;

bool load_file( const char *name )
{
	FILE *fp = fopen( name, "rb" );
	if( fp == 0 )
	{
		return false;
	}
	
	file_info fi;

	fseek( fp, 0, SEEK_END );
	fi.size = (size_t)ftell( fp );
	fseek( fp, 0, SEEK_SET );

	fi.imported = false;
	fi.content = (char*) malloc( fi.size );
	fread( fi.content, fi.size, 1, fp );

	fclose( fp );

	// save it 
	gFiles[name] = fi;

	return true;
}

bool load_files( const char *cfg_file )
{
	FILE *fp = fopen( cfg_file, "r" );
	if( fp == 0 )
	{
		return false;
	}
	
	char name[256];
	while( feof( fp ) == 0 )
	{
		fscanf( fp, "%s", name );
		if( !load_file( name ) )
		{
			fprintf( stderr, "warning : load script file [%s] failed\n", name );
		}
		else
		{
			printf( "info : load script file [%s] ok\n", name );
		}
	}

	fclose( fp );

	return true;
}

bool import_script( lua_State *L, file_info &fi, const std::string &name )
{
	// compile and execute it
	if( luaL_loadbuffer( L, fi.content, fi.size, 0 ) || lua_pcall( L, 0, 0, 0 ) )
	{
		// error
		fprintf( stderr, "error : import script [%s] error : %s\n", name.c_str(), lua_tostring( L, -1 ) );
		return false;
	}
	
	fi.imported = true;
	return true;
}

static int import( lua_State *L )
{
	const char *str = luaL_checkstring( L, -1 );
	// get the script file
	file_list_type::iterator it = gFiles.find( str );
	if( it == gFiles.end() )
	{
		// error
		fprintf( stderr, "error : import script file [%s] failed\n", str );
		return 0;
	}

	file_info &fi = it->second ;
	if( fi.imported )
	{
		printf( "info : script file [%s] has already imported\n", str );
		return 0;
	}

	// import it.
	import_script( L, fi, str );
	
	return 0;
}

bool init_scripts( lua_State *L )
{
	bool ret = true;
	// import all scripts into lua_State
	for( file_list_type::iterator it = gFiles.begin(); it != gFiles.end(); ++ it )
	{
		ret = ret && import_script( L, it->second, it->first );
	}

	return ret;
}

void execute_fn( lua_State *L, const char *fn_name )
{
	// get the table name
	const char *dot = strchr( fn_name, '.' );
	if( dot == 0 )
	{
		fprintf( stderr, "error : invalid fn_name\n" );
		return ;
	}

	char table[256];
	strncpy( table, fn_name, dot - fn_name );
	table[dot-fn_name] = 0;

	char fn[256];
	strcpy( fn, dot + 1 );

	// get the table 
	lua_getglobal( L, table );
	if( !lua_istable( L, -1 ) )
	{
		fprintf( stderr, "error : [%s] is not a table\n", table );
		return ;
	}

	// get the function
	lua_pushstring( L, fn );
	lua_gettable( L, -2 ); // the table is on the -2 index
	if( !lua_isfunction( L, -1 ) )
	{
		fprintf( stderr, "error : [%s] is not a function\n", fn );
		return ;
	}

	// i suppose the function takes no arguments here, just for testing purpose.
	lua_pcall( L, 0, 0, 0 );
}

int main()
{
	lua_State *L = lua_open();
	luaL_openlibs( L );
	
	// register the 'import' function
	lua_register( L, "import", import );

	if( !load_files( "tconfig.ini" ) )
	{
		fprintf( stderr, "error : load [tconfig.ini] failed\n" );
		exit( -1 );
	}

	if( !init_scripts( L ) )
	{
		fprintf( stderr, "error : init scripts failed\n" );
		exit( -1 );
	}
	
	printf( "info : init scripts ok\nEnter function to execute\n" );

	char fn_name[256];
	while( scanf( "%s", fn_name ) )
	{
		execute_fn( L, fn_name );
	}

	lua_close( L );
	return 0;
}
