///
/// @file table_bind.cpp
///
#include <stdlib.h>
#include <lua5.1/lua.hpp>
#include "table.h"

#define MT_NAME "ParamTable.MT"
#define check_PT(L) *(ParamTable**)luaL_checkudata (L, 1, MT_NAME)
#define DEBUG printf

// value adapter, test only
static void PushValue (lua_State *L, const Value *val) {
    val ? lua_pushnumber (L, (lua_Number) *val) : lua_pushnil (L);
}

static Value PopValue (lua_State *L, int idx) {
    if (lua_isnil (L, idx)) return -1;
    if (lua_isnumber (L, idx)) return lua_tonumber (L, idx);
    return 0;
}
//

int PT_New (lua_State *L) {
    DEBUG ("PT_New\n");
    void **up = (void**) lua_newuserdata (L, sizeof(void*));
    *up = new ParamTable ();
    luaL_getmetatable (L, MT_NAME);
    lua_setmetatable (L, -2);
    return 1;
}

int PT_Delete (lua_State *L) {
    DEBUG ("PT_Delete\n");
    ParamTable *pt = check_PT (L);
    delete pt;
    return 0;
}

int PT_Get (lua_State *L) {
    ParamTable *pt = check_PT (L);
    const char *key = luaL_checkstring (L, 2);
    luaL_argcheck (L, key, 2, "a valid string expected");
    DEBUG ("PT_Get: %s\n", key);
    const Value *val = pt->Get (key);
    PushValue (L, val);
    return 1;
}

int PT_Set (lua_State *L) {
    ParamTable *pt = check_PT (L);
    const char *key = luaL_checkstring (L, 2);
    luaL_argcheck (L, key, 2, "a valid string expected");
    const Value &val = PopValue (L, 3);
    DEBUG ("PT_Set: %s\n", key);
    pt->Set (key, val);
    return 0;
}

static const struct luaL_Reg pt_lib_f [] = {
    {"__newindex", PT_Set},
    {"__index", PT_Get},
    {"__gc", PT_Delete},
    {NULL, NULL}
};

static const struct luaL_Reg pt_lib_m [] = {
    {"new", PT_New},
    {NULL, NULL}
};

/*
  Usage in Lua:
    local pt = ParamTable.new() 
    local v = pt.name -- __index metamethod -> mt.__index -> PT_Get
    pt.name = "hello" -- __newindex metamethod -> mt.__newindex -> PT_Set
    -- pt will be __gc -> __gc metamethod -> mt.__gc -> PT_Delete
*/
void PT_OpenLib (lua_State *L) {
    luaL_newmetatable (L, MT_NAME);
    luaL_register (L, NULL, pt_lib_f);
    luaL_register (L, "ParamTable", pt_lib_m);
}

