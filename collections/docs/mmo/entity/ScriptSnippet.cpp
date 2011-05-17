///
/// @file ScriptSnippet.cpp
/// @author Kevin Lynx
/// @brief Script related functions. Snippet codes, cannot be compiled
///

#define CheckArgReturn(exp, msg) \
    if (!(exp)) { \
        lua_pushnil (L); \
        lua_pushstring (L, msg); \
        return 2; \
    }

int SetPropertyTemplate (lua_State *L)
{
    int type = luaL_checkint (L, 1);
    ParamTable *pt = PT_CheckGet (L, 2);
    CheckArgReturn (pt, "invalid property template");
    GetInst (PropertyTemplate).Add (type, *pt);
    lua_pushboolean (L, 1);
    return 1;
}

