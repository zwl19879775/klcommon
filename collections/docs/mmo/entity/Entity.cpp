///
/// @file Entity.cpp
/// @author Kevin Lynx
/// 
//#include "stdafx.h"
#include "Entity.h"
#include "Property.h"
#include "IComponent.h"

Entity::~Entity ()
{
    Destroy ();
}

bool Entity::AddProperty (const std::string &name, const GValue &initval)
{
    if (HasProperty (name)) 
    {
        ELogWarn ("The property (%s) already exists", name.c_str ());
        return false;
    }
    ELogDebug ("Add a property (%s)", name.c_str ());
    m_properties[name] = new Property (name, initval);
    return true;
}

void Entity::RemoveProperty (const std::string &name)
{
    PropertyTable::iterator it = m_properties.find (name);
    if (it != m_properties.end ())
    {
        ELogDebug ("Remove a property (%s)", it->first.c_str ());
        delete it->second;
    }
}

bool Entity::HasProperty (const std::string &name) const
{
    return m_properties.find (name) != m_properties.end ();
}

GValue Entity::GetPropertyVal (const std::string &name) const
{
    PropertyTable::const_iterator it = m_properties.find (name);
    if (it == m_properties.end ())
    {
        ELogWarn ("Query a non-exist property (%s) value", name.c_str ());
        return GValue ();
    }
    return it->second->Get ();
}

void Entity::SetPropertyVal (const std::string &name, const GValue &val)
{
    PropertyTable::iterator it = m_properties.find (name);
    if (it == m_properties.end ())
    {
        ELogInfo ("Set a non-exist property (%s) value, add it", name.c_str ());
        AddProperty (name, val);
        return ;
    }
    ELogInfo ("Set a property (%s) value", name.c_str ());
    it->second->Set (val);
}

bool Entity::AddPropertyObserver (const std::string &name, IComponent *observer)
{
    PropertyTable::iterator it = m_properties.find (name);
    if (it == m_properties.end ())
    {
        ELogWarn ("Add observer (%s) to a non-exist property", observer->Name ().c_str ());
        return false;
    }
    ELogDebug ("Add observer (%s) to property (%s)", observer->Name ().c_str(), name.c_str ());
    it->second->AddObserver (observer);
    return true;
}

void Entity::Destroy ()
{
    ELogDebug ("Destroy properties and components");
    TRAVERSE_MAP (PropertyTable, m_properties, 
        delete I_VALUE )
    TRAVERSE_MAP (ComponentTable, m_components,
        delete I_VALUE )
}

