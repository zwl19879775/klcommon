///
/// @file IComponent.h
/// @author Kevin Lynx
/// 
#ifndef ___ICOMPONENT_H_
#define ___ICOMPONENT_H_

#include <vector>
#include <string>
#include "EntityBase.h"
#include "SharedPropertyArray.h"

class Entity;

/// A component is a class which wraps operations (operate on properties) in it.
class IComponent
{
public:
    IComponent (Entity *entity, const std::string &name) : 
        m_name (name), m_entity (entity) 
    { 
    }

    virtual ~IComponent () { }

    /// Called after this component has been added to the entity. It can add shared properties
    /// in this function.
    virtual void OnAdd () { }

    /// Called before this component removed from the entity.
    virtual void OnRemove () { }

    /// Provide an uniform property read operation. Return the specified property value,
    /// if the property does not exist, return nil value.
    virtual GValue GetPropertyVal (const std::string &name) const { return GValue (); }

    /// Set the property value, if success return true.
    virtual bool SetPropertyVal (const std::string &name, const GValue &val) { return false; }

    /// Increase/Decrease a property value, it only applys to a number property value.
    virtual bool IncPropertyVal (const std::string &name, double inc) { return false; }

    /// Return the component name.
    const std::string &Name () const { return m_name; }

    /// Get the corresponding entity.
    Entity *GetEntity () const { return m_entity; }

protected:
    /// A helper function to add shared properties from a static array.
    void AddSharedProperties (const SharedProArray &lst);
    
    /// Remove a list of shared properties.
    void RemoveSharedProperties (const SharedProArray &lst);

    /// Add a shared property.
    void AddSharedProperty (const std::string &name);

    /// Remove a shared property.
    void RemoveSharedProperty (const std::string &name);

    /// A helper function to add shared properties from a vector<string> container.
    void AddSharedProperties (const std::vector<std::string> &lst);

    /// Remove a list of shared properties.
    void RemoveSharedProperties (const std::vector<std::string> &lst);

protected:
    std::string m_name;
    Entity *m_entity;
};

#endif

