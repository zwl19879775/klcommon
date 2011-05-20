///
/// @file Entity.cpp
/// @author Kevin Lynx
/// 
//#include "stdafx.h"
#include "IComponent.h"
#include "Entity.h"

Entity::~Entity() 
{
    ELogDebug ("Destroy all components");
    TRAVERSE_MAP (ComponentTable, m_components, delete I_VALUE)
}

void Entity::AddComponent (IComponent *com)
{
    if (HasComponent (com->Name ()))
    {
        ELogWarn ("Already has component (%s)", com->Name ().c_str ());
        return;
    }
    ELogDebug ("Add a new component (%s)", com->Name ().c_str ());
    m_components[com->Name ()] = com;
    com->OnAdd ();
}

void Entity::RemoveComponent (const std::string &name)
{
    ComponentTable::iterator it = m_components.find (name);
    if (it != m_components.end ())
    {
        ELogDebug ("Remove a component (%s)", it->first.c_str ());
        it->second->OnRemove ();
        delete it->second;
    }
}

IComponent *Entity::GetComponent (const std::string &name) const
{
    ComponentTable::const_iterator it = m_components.find (name);
    if (it == m_components.end ())
    {
        ELogWarn ("Request a null component (%s)", name.c_str ());
        return NULL;
    }
    return it->second;
}

GValue Entity::GetSharedPropertyVal (const std::string &name) const
{
    PropertyTable::const_iterator it = m_sharedProperties.find (name);
    if (it == m_sharedProperties.end ())
    {
        ELogWarn ("Not found the shared property (%s) value", name.c_str ());
        return GValue ();
    }
    return it->second->GetPropertyVal (name);
}

bool Entity::SetSharedPropertyVal (const std::string &name, const GValue &val)
{
    PropertyTable::iterator it = m_sharedProperties.find (name);
    if (it == m_sharedProperties.end ())
    {
        ELogWarn ("Not found the shared property (%s) value", name.c_str ());
        return false;
    }
    it->second->SetPropertyVal (name, val);
    return true;
}

bool Entity::IncSharedPropertyVal (const std::string &name, double inc)
{
    PropertyTable::iterator it = m_sharedProperties.find (name);
    if (it == m_sharedProperties.end ())
    {
        ELogWarn ("Not found the shared property (%s) value", name.c_str ());
        return false;
    }
    it->second->IncPropertyVal (name, inc);
    return true;
}

void Entity::AddSharedProperty (const std::string &name, IComponent *com)
{
    PropertyTable::iterator it = m_sharedProperties.find (name);
    if (it != m_sharedProperties.end ())
    {
        ELogWarn ("Conflict shared property name (%s)", name.c_str ());
        return ;
    }
    ELogDebug ("Add a shared property (%s)", name.c_str ());
    m_sharedProperties[name] = com;
}

void Entity::RemoveSharedProperty (const std::string &name)
{
    ELogDebug ("Remove a shared property (%s)", name.c_str ());
    m_sharedProperties.erase (name);
}

