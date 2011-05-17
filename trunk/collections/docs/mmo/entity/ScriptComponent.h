///
/// @file ScriptComponent.h
/// @author Kevin Lynx
///
#ifndef ___SCRIPT_COMPONENT_H_
#define ___SCRIPT_COMPONENT_H_

#include "IComponent.h"

/// Script component wraps a lua table as a component, this component is as usual, 
/// that can be added into an entity.
class ScriptComponent : public IComponent
{
public:
    /// Construct a ScriptComponent, the `sref` is the return value from
    /// `ScriptComponent::GetScriptRef`, and it must be valid.
    /// This function will call `OnCreated` in script.
    ScriptComponent (Entity *entity, lua_State *L, int sref);

    ~ScriptComponent ();

    /// Retrieve a script table component from stack
    static int GetScriptRef (lua_State *L, int idx);

    /// Release the reference returned by `GetScriptRef`. This function will be called
    /// in the destructor.
    static void DropScriptRef (lua_State *L, int ref);

    /// Call script 'Process` function.
    virtual bool Process (const ArgsT *args);

    /// Call script 'OnNotify' function.
    virtual void OnNotify (const Property &p, const ArgsT *args);
    
    /// Call script 'OnMessage' function.
    virtual void OnMessage (const ArgsT *args);
private:
    /// Call the script method
    bool CallMethod (const char *method, const ArgsT *args);
private:
    int m_sref;
    lua_State *m_L;
};

#endif

