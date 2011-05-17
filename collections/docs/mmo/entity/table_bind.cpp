///
/// @file table_bind.cpp
/// @author Kevin Lynx
/// @brief Bind the c/c++ key-value table in lua.
///
#include <stdlib.h>
#include <lua5.1/lua.hpp>
#include "table.h"
#include "gvalue_util.h"

#define MT_NAME "ParamTable.MT"
#define check_PT(L, idx) *(ParamTable**)luaL_checkudata (L, idx, MT_NAME)
#define DEBUG printf

// If pt.sub_pt is also ParamTable, everytime you write pt.sub_pt in lua,
// it will create a new lua fullusedata. And the worse thing is, several 
// usedata will reference to the same ParamTable, so to avoid the multiple delete,
// i mark a reference count in ParamTable.
#define REF_PTR(pt) ((int*)((char*) pt + sizeof(ParamTable)))
static ParamTable *create_pt () {
    void *p = malloc (sizeof(ParamTable)+sizeof(void*));
    int *ref = REF_PTR (p);
    *ref = 1;
    DEBUG ("create a new pt: %p\n", p);
    return new (p) ParamTable();
}

static void delete_pt (ParamTable *p) {
    int *ref = REF_PTR (p);
    *ref = *ref - 1;
    DEBUG ("dec ref count %p:%d\n", p, *ref);
    if (*ref == 0) {
        DEBUG ("delete the pt: %p\n", p);
        p->~ParamTable();
        free (p);
    }
}

static ParamTable *ref_pt (ParamTable *p) {
    int *ref = REF_PTR (p);
    *ref = *ref + 1;
    DEBUG ("inc ref count %p:%d\n", p, *ref);
    return p;
}

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
            new_metatable (L, ref_pt (CastParamTable (*val)));
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
        ParamTable *pt = *(ParamTable**) luaL_checkudata (L, idx, MT_NAME);
        return CreateGValue (pt);
    }
    return Value();
}

int PT_New (lua_State *L) {
    DEBUG ("PT_New\n");
    new_metatable (L, create_pt ());
    return 1;
}

int PT_Delete (lua_State *L) {
    DEBUG ("PT_Delete\n");
    ParamTable *pt = check_PT (L, 1);
    delete_pt (pt);
    return 0;
}

int PT_Get (lua_State *L) {
    ParamTable *pt = check_PT (L, 1);
    const char *key = luaL_checkstring (L, 2);
    luaL_argcheck (L, key, 2, "a valid string expected");
    DEBUG ("PT_Get: %s\n", key);
    const Value *val = pt->Get (key);
    PushValue (L, val);
    return 1;
}

int PT_Set (lua_State *L) {
    ParamTable *pt = check_PT (L, 1);
    const char *key = luaL_checkstring (L, 2);
    luaL_argcheck (L, key, 2, "a valid string expected");
    const Value &val = PopValue (L, 3);
    DEBUG ("PT_Set: %s\n", key);
    pt->Set (key, val);
    return 0;
}

int PT_Equal (lua_State *L) {
    ParamTable *p1 = check_PT (L, 1);
    ParamTable *p2 = check_PT (L, 2);
    int r = p1 == p2 ? 1 : 0;
    lua_pushboolean (L, r);
    return 1;
}

int PT_Size (lua_State *L) {
    ParamTable *pt = check_PT (L, 1);
    lua_pushinteger (L, (lua_Integer) pt->Size());
    return 1;
}

int PT_Print (lua_State *L) {
    ParamTable *pt = check_PT (L, 1);
    lua_pushfstring (L, "ParamTable[%p:%d]", pt, pt->Size());
    return 1;
}

typedef ParamTable::ValTable::const_iterator Iter;

int PT_Iter (lua_State *L) {
    ParamTable *pt = (ParamTable*) lua_touserdata (L, lua_upvalueindex (1));
    Iter *it = (Iter*) lua_touserdata (L, lua_upvalueindex (2));
    if (*it == pt->Values().end()) {
        delete it;
        lua_pushnil (L);
        return 1;
    }
    lua_pushstring (L, (*it)->first.c_str());
    PushValue (L, &(*it)->second);
    ++ (*it);
    return 2;
}

int PT_Traverse (lua_State *L) {
    ParamTable *pt = check_PT (L, 1);
    Iter *it = new Iter(pt->Values().begin());
    lua_pushlightuserdata (L, pt);
    lua_pushlightuserdata (L, it);
    lua_pushcclosure (L, PT_Iter, 2);
    return 1;
}

static const struct luaL_Reg pt_lib_f [] = {
    {"__newindex", PT_Set},
    {"__index", PT_Get},
    {"__gc", PT_Delete},
    {"__eq", PT_Equal},
    {"__len", PT_Size},
    {"__tostring", PT_Print},
    {NULL, NULL}
};

static const struct luaL_Reg pt_lib_m [] = {
    {"new", PT_New},
    {"values", PT_Traverse},
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

