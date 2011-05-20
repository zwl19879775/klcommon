///
/// @file Entity.h
/// @author Kevin Lynx
/// 
#ifndef ___ENTITY_H_
#define ___ENTITY_H_

#include <string>
#include <map>

class IComponent;

/// test purpose
typedef long CGUID;

/// An entity is an abstract game object, which only has some components. And components 
/// maintain the properties. 
class Entity
{
public:
    /// <component-name, Component>
    typedef std::map<std::string, IComponent*> ComponentTable;
    /// <property-name, Component>, to store shared properties
    typedef std::map<std::string, IComponent*> PropertyTable;
public:
    Entity (int type, const CGUID &id) : m_type (type), m_id (id)
    {
    }

    /// It will delete all components and reset component/property table.
    ~Entity ();

    /// Add a component, and add these shared properties returned from the component.
    void AddComponent (IComponent *com);

    /// Remove a component by its name.
    void RemoveComponent (const std::string &name);

    /// Get a component by its name.
    IComponent *GetComponent (const std::string &name) const;

    /// Use dynamic_cast to cast an abstract component to concrete compoent.
    template <typename ComT>
    ComT *GetComponent (const std::string &name) const;

    /// Find the corresponding component by the property name, and call com->GetPropertyVal .
    /// If not found return nil value.
    GValue GetSharedPropertyVal (const std::string &name) const;

    /// Set a shared property value, if not found the property, return false.
    /// If it's possible, do not call this function to set the value because it's a bit
    /// overhead. The better way is to find the component first and then set the value.
    bool SetSharedPropertyVal (const std::string &name, const GValue &val);

    /// Increase/Decrease a property value.
    bool IncSharedPropertyVal (const std::string &name, double inc);

    /// Check whether this entity has a specified component.
    bool HasComponent (const std::string &name) const { return GetComponent (name) != NULL; }

    int Type () const { return m_type; }
    const CGUID &ID () const { return m_id; }

private:
    /// Add a shared property into PropertyTable
    void AddSharedProperty (const std::string &name, IComponent *com);
    
    /// Remove a shared property by the property name.
    void RemoveSharedProperty (const std::string &name);

    /// So that the Component can add/remove shared properties in their scope.
    friend class IComponent;
private:
    ComponentTable m_components;
    PropertyTable m_sharedProperties;
    int m_type;
    CGUID m_id;
};

template <typename ComT>
ComT *Entity::GetComponent (const std::string &name) const
{
    ComT *ret = dynamic_cast<ComT*> (GetComponent (name));
    if (!ret)
    {
        ELogWarn ("Request a component (%s) with un-match type", name.c_str ());
    }
    return ret;
}

#endif

