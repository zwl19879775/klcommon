///
/// @file IComponent.h
/// @author Kevin Lynx
/// 
#ifndef ___ICOMPONENT_H_
#define ___ICOMPONENT_H_

#include <vector>
#include <string>
#include "EntityBase.h"

class Entity;

/// A component is a class which wraps operations (operate on properties) in it.
class IComponent
{
public:
    typedef std::vector<std::string> SharedPropertyList;
public:
    IComponent (Entity *entity, const std::string &name) : 
        m_name (name), m_entity (entity) 
    { 
    }

    virtual ~IComponent () { }

    /// When a component is added to an entity, this function will get called, and
    /// the entity will add these shared properties returned by this function to the
    /// shared properties table.
    virtual SharedPropertyList GetSharedProperties () const { return SharedPropertyList (); }

    /// Provide an uniform property read operation. Return the specified property value,
    /// if the property does not exist, return nil value.
    virtual GValue GetPropertyVal (const std::string &name) const { return GValue (); }

    /// Set the property value, if success return true.
    virtual bool SetPropertyVal (const std::string &name, const GValue &val) { return false; }

    /// Return the component name.
    const std::string &Name () const { return m_name; }

    /// Get the corresponding entity.
    Entity *GetEntity () const { return m_entity; }
protected:
    std::string m_name;
    Entity *m_entity;
};

#endif

