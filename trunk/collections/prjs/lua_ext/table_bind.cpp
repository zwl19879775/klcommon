///
/// @file table_bind.cpp
///
#include <stdlib.h>
#include <lua5.1/lua.hpp>
#include "table.h"
#include "gvalue_util.h"

#define MT_NAME "ParamTable.MT"
#define check_PT(L) *(ParamTable**)luaL_checkudata (L, 1, MT_NAME)
#define DEBUG printf

static void new_metatable (lua_State *L, ParamTable *pt) {
    DEBUG ("new_metatable\n");
    void **up = (void**) lua_newuserdata (L, sizeof(void*));
    *up = pt;
    luaL_getmetatable (L, MT_NAME);
    lua_setmetatable (L, -2);
}

// value adapter
static void PushValue (lua_State *L, const Value *val) {
    switch (val->Type()) {
        case Value::NIL: lua_pushnil (L); break;
        case Value::NUMBER: lua_pushnumber (L, (lua_Number) val->GetNumber()); break;
        case Value::BOOLEAN: lua_pushboolean (L, val->GetBool() ? 1 : 0); break;
        case EXT_STRING: lua_pushstring (L, CastString (*val).c_str()); break;
        case EXT_PARAMTABLE: {
            // new everytime you want a ParamTable.
            new_metatable (L, CastParamTable (*val));
         }
        break;
        default: lua_pushnil (L); break;
    }
}

static Value PopValue (lua_State *L, int idx) {
    if (lua_isnil (L, idx)) return Value ();
    if (lua_isnumber (L, idx)) return Value ((double)lua_tonumber (L, idx));
    if (lua_isboolean (L, idx)) return Value (lua_toboolean (L, idx) ? true : false);
    if (lua_isstring (L, idx)) return CreateGValue (lua_tostring (L, idx));
    if (lua_isuserdata (L, idx)) {
        void **u = (void**) luaL_checkudata (L, idx, MT_NAME);
        ParamTable *pt = *(ParamTable**) u;
        /* what can i do? Because lua cannot map fulluserdata to a c/c++ object*/
        *u = NULL; 
        return CreateGValue (pt);
    }
    return Value();
}
//

int PT_New (lua_State *L) {
    DEBUG ("PT_New\n");
    new_metatable (L, new ParamTable ());
    return 1;
}

int PT_Delete (lua_State *L) {
    DEBUG ("PT_Delete\n");
    ParamTable *pt = check_PT (L);
    if (pt) /* because i make it null somewhere */
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

