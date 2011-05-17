///
/// @file ScriptComponent.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "ScriptComponent.h"

ScriptComponent::ScriptComponent (Entity *entity, lua_State *L, int sref) :
    IComponent (entity, "ScriptComponent")
{
    m_L = L;
    m_sref = sref;
    if (m_sref == LUA_REFNIL)
    {
        ELogWarn ("Referenced to a nil script component");
    }
    ArgsT args;
    args.Set ("entity", CreateGValue (entity->ID ()));
    CallMethod ("OnCreated", &args);
}

ScriptComponent::~ScriptComponent ()
{
    DropScriptRef (m_L, m_sref);
}

int ScriptComponent::GetScriptRef (lua_State *L, int idx)
{
    lua_pushvalue (L, idx);
    if (!lua_istable (L, -1)) return LUA_REFNIL;
    return luaL_ref (L, LUA_REGISTRYINDEX);
}

void ScriptComponent::DropScriptRef (lua_State *L, int ref)
{
    luaL_unref (L, LUA_REGISTRYINDEX, ref);
}

bool ScriptComponent::Process (const ArgsT *args)
{
    return CallMethod ("Process", args);
}

void ScriptComponent::OnNotify (const Property &p, const ArgsT *args)
{
    ArgsT nargs;
    nargs.Set ("old-val", args->Get ("old-val"));
    nargs.Set ("name", CreateGValue (p.Name ()));
    nargs.Set ("value", CreateGValue (p.Get ()));
    CallMethod ("OnNotify", &nargs);
}

void ScriptComponent::OnMessage (const ArgsT *args)
{
    CallMethod ("OnMessage", args);
}

bool ScriptComponent::CallMethod (const char *method, const ArgsT *args)
{
    if (m_sref == LUA_REFNIL) return false;
    // get the table
    lua_rawgeti (m_L, LUA_REGISTRYINDEX, m_sref);
    // get the function in the table
    lua_getfield (m_L, -1, method);
    if (!lua_isfunction (m_L, -1)) 
    {
        ELogError ("Not found the method (%s)", method);
        lua_pop (m_L, 2); // nil and the table
        return false;
    }
    // push the argument table
    if (args) PT_Push (m_L, args);
    // call it.
    int ret = lua_pcall (m_L, 1, 0, 0);
    if (ret != 0)
    {
        ELogError ("Execute method (%s) error", method);
        return false;
    }
    return true;
}

