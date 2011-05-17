///
/// @file IComponent.h
/// @author Kevin Lynx
/// 
#ifndef ___ICOMPONENT_H_
#define ___ICOMPONENT_H_

#include <string.h>
#include "EntityBase.h"

class ComMessage;
class Property;
class Entity;

/// A component is a class which wraps operations (operate on entity properties) in it.
/// A component can be called directly by function `Process`, also can be called by
/// `OnNotify` as an observer.
class IComponent
{
public:
    IComponent (Entity *entity, const std::string &name) : 
        m_name (name), m_entity (entity) 
    { 
    }

    virtual ~IComponent () { }

    /// If the process success, return true.
    virtual bool Process (const ArgsT *args) { return false; }

    /// When this component is an observer, when the property value changed, it 
    /// will call this function. Arguments:  old-val -> property_old_value
    virtual void OnNotify (const Property &property, const ArgsT *args) {}

    /// Process a message assoicated with this component.
    virtual void OnMessage (const ArgsT *args) {}

    /// Return the component name.
    const std::string &Name () const { return m_name; }

    /// Get the corresponding entity.
    Entity *GetEntity () const { return m_entity; }

protected:
    std::string m_name;
    Entity *m_entity;
};

#endif

