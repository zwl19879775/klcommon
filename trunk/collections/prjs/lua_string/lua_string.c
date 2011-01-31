/**
 * bind string_converter to lua
 * Kevin Lynx
 * 1.30.2011
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stringencoder.h"
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>

int luas_u2g(lua_State *L) {
    size_t len = 0;
    char *out = 0;
    const char *inputs = luaL_checklstring(L, 1, &len);
    out = (char*) malloc(len+1); /* gbk usually spent less size than utf8 ? */
    utf82dbcs("GBK", inputs, len+1, out, len+1);
    lua_pushlstring(L, out, strlen(out));
    free(out);
    return 1;
}

int luas_g2u(lua_State *L) {
    size_t len = 0;
    char *out = 0;
    const char *inputs = luaL_checklstring(L, 1, &len);
    out = (char*) malloc(len*2); /* utf8 spent more size than gbk */
    dbcs2utf8("GBK", inputs, len+1, out, len*2);
    lua_pushlstring(L, out, strlen(out));
    free(out);
    return 1;
}

static luaL_reg funcs[] = {
    { "u2g", luas_u2g },
    { "g2u", luas_g2u },
    { 0, 0 }
};

extern int luaopen_stringconv(lua_State *L) {
    luaL_openlib(L, "stringconv", funcs, 0);
    return 1;
}

