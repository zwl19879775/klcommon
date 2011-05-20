///
/// @file ScriptProperties.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "ScriptProperties.h"
#include "ScriptPropertyList.h"
#include "../Entity.h"

ScriptProperties::ScriptProperties (Entity *entity) :
    IComponent (entity, "ScriptProperties")
{
}

ScriptProperties::~ScriptProperties ()
{
}

void ScriptProperties::OnAdd ()
{
    // Load the property table first
    InitProperties ();
    // Add shared properties
    TRAVERSE_MAP (PropertyTable, m_properties,
        AddSharedProperty (I_KEY));
}

void ScriptProperties::OnRemove ()
{
    // Remove shared properties
    TRAVERSE_MAP (PropertyTable, m_properties,
        RemoveSharedProperty (I_KEY));
}

GValue ScriptProperties::GetPropertyVal (const std::string &name) const
{
    PropertyTable::const_iterator it = m_properties.find (name);
    if (it != m_properties.end ())
    {
        return it->second;
    }
    ELogWarn ("Request a non-existed property (%s) value", name.c_str ());
    return GValue ();
}

bool ScriptProperties::SetPropertyVal (const std::string &name, const GValue &val)
{
    PropertyTable::iterator it = m_properties.find (name);
    if (it == m_properties.end ())
    {
        ELogWarn ("Set a non-existed property (%s) value", name.c_str ());
        return false;
    }
    it->second = val;
    return true;
}

bool ScriptProperties::IncPropertyVal (const std::string &name, double inc)
{
    PropertyTable::iterator it = m_properties.find (name);
    if (it == m_properties.end ())
    {
        ELogWarn ("Inc a non-existed property (%s) value", name.c_str ());
        return false;
    }
    GValue &val = it->second;
    if (val.Type () != GValue::NUMBER)
    {
        ELogWarn ("Type mismatched, apply (%d) to number", val.Type ());
        return false;
    }
    val.Set (val.GetNumber () + inc);
    return true;
}

bool ScriptProperties::InitProperties ()
{
    const SharedProArray &lst = SP_Get ();
    // initialize all propertis as a number
    TRAVERSE_SHAREDPRO_ARRAY (lst, 
        m_properties[SA_VALUE] = GValue (0.0));
    return true;
}

