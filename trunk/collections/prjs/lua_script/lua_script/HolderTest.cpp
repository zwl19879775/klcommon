///
///
///
#ifdef HOLDER_TEST
#include "luainc.h"
#include <stdio.h>
#include "ScriptHolder.h"
#include "utils/file_loader.h"

#define SCRIPT_NUM (2)

void LoadScripts( ScriptHolder *holder )
{
	char file[512];
	for( int i = 0; i < SCRIPT_NUM; ++ i )
	{
		sprintf( file, "scripts\\%d.lua", i + 1 );
		FileLoader loader;
		loader.Load( file, FileLoader::BINARY );
		FileLoader::RawData data = loader.GetRawData();
		holder->Add( file, (char*) data.buf, data.size );
	}
}

void TestCall( ScriptHolder *holder, lua_State *L )
{
	holder->Get( "scripts\\1.lua" );
	lua_pcall( L, 0, 0, 0 );

	holder->Get( "scripts\\2.lua" );
	lua_pcall( L, 0, 0, 0 );
}

int main()
{
	lua_State *L = lua_open();
	luaopen_base( L );
	ScriptHolder holder( L );
	holder.Init();
	LoadScripts( &holder );

	TestCall( &holder, L );

	lua_close( L );
	return 0;
}

#endif
