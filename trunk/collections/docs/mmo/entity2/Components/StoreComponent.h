///
/// @file StoreComponent.h
/// @author Kevin Lynx
///
#ifndef ___STORE_COMPONENT_H_
#define ___STORE_COMPONENT_H_

#include "../RefMembers.h"
#include "../IComponent.h"

/// StoreComponent actually is an abstract component, it provides a mechanism
/// other components to store some values.
class StoreComponent : public IComponent
{
public:
    StoreComponent (Entity *entity, const std::string &name) :
        IComponent (entity, name)
    {
    }
    
    virtual ~StoreComponent () { }

    virtual GValue GetPropertyVal (const std::string &name) const
    {
        return m_refmem.GetValue (name);
    }

    virtual bool SetPropertyVal (const std::string &name, const GValue &val)
    {
        return m_refmem.SetValue (name, val);
    }

    virtual bool IncPropertyVal (const std::string &name, double inc)
    {
        return m_refmem.IncValue (name, inc);
    }

    virtual void Reset () = 0;
protected:
    RefMembers m_refmem;
};

#endif

