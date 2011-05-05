///
/// @file test_table.cpp
/// @brief test ParamTable in Lua
/// @author Kevin Lynx
/// @date 5.5.2011
///
#include <lua5.1/lua.hpp>
#include <stdio.h>
#include "table_bind.h"

int main (int argc, char **argv) {
    if (argc < 2) {
        fprintf (stderr, "Usage: %s lua-file.\n", argv[0]);
        return -1;
    }
    lua_State *L = luaL_newstate();
    luaL_openlibs (L);
    PT_OpenLib (L);
	if (luaL_loadfile(L, argv[1]) != 0 || lua_pcall(L, 0, 0, 0)) {
		fprintf(stderr, "%s\n", lua_tostring(L, -1));
		return -1;
	}
    lua_close (L);
    return 0;
}

