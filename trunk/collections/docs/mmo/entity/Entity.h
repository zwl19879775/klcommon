///
/// @file Entity.h
/// @author Kevin Lynx
/// 
#ifndef ___ENTITY_H_
#define ___ENTITY_H_

#include <string>
#include <map>
#include "EntityBase.h"

class Property;
class IComponent;
class ComMessage;

/// test purpose
typedef long CGUID;

/// An entity is an abstract game object, which will have some properties and some
/// components. Properties and components can only be added when an entity is created,
/// so it does NOT support add/remove properties/components at the runtime -- is this
/// the right design (i'm not sure yet)?
class Entity
{
public:
    /// Build a key-value property table.
    typedef std::map<std::string, Property*> PropertyTable;
    /// Also has a component table.
    typedef std::map<std::string, IComponent*> ComponentTable;
public:
    Entity (int type, const CGUID &id) : m_type (type), m_id (id)
    {
    }

    /// It will delete all components and reset component/property table.
    ~Entity ();

    /// Add a property and set its initial value, if the property exist return false
    bool AddProperty (const std::string &name, const GValue &initval);

    /// Remove the property from the property table.
    void RemoveProperty (const std::string &name);

    /// Check whether a specified property exists.
    bool HasProperty (const std::string &name) const;

    /// Get a property value, if the property does not exist, return a nil value. 
    /// See `GValue` document about `nil value`.
    GValue GetPropertyVal (const std::string &name) const;

    /// Set a property value, if the property does not exist, this function will
    /// add a new property and set the `val` as its initial value.
    void SetPropertyVal (const std::string &name, const GValue &val);

    /// Append an observer to a property, an observer is a component which will be notified
    /// whenver the observed property value changed. If the property does not exist, 
    /// this function returns false.
    bool AddPropertyObserver (const std::string &name, IComponent *observer);

    /// Add a component with the component name. If the component already exists, 
    /// return false.
    bool AddComponent (IComponent *com);

    /// If the component does not exist, return NULL.
    IComponent *GetComponent (const std::string &name) const;

    /// Receive a message, it will find the corresponding component and pass the message 
    /// to it. If the component does not exist, return false.
    bool RecvMessage (const ComMessage &msg);

    int Type () const { return m_type; }
    const CGUID &ID () const { return m_id; }

private:
    /// Delete all properties and components.
    void Destroy ();
private:
    int m_type;
    CGUID m_id;
    PropertyTable m_properties;
    ComponentTable m_components;
};

#endif

