///
/// @file PropertyTemplate.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "PropertyTemplate.h"
#include "Entity.h"

void PropertyTemplate::Add (int type, const ArgsT &pt)
{
    TemplateTable::iterator it = m_ptable.find (type);
    if (it != m_ptable.end ())
    {
        ELogInfo ("The property template (%d) exist, update it", type);
        Update (it->second, pt);
    }
    else
    {
        ELogInfo ("Add a new property template (%d)", type);
        ArgsT *args = new ArgsT (pt);
        m_ptable[type] = args;
    }
}

bool PropertyTemplate::AddToEntity (Entity *entity) const
{
    TemplateTable::const_iterator it = m_ptable.find (entity->Type ());
    if (it == m_ptable.end ())
    {
        ELogWarn ("The property template (%d) does not exist", entity->Type ());
        return false;
    }
    ELogDebug ("Add properties to an entity by template (%d)", entity->Type ());
    const ArgsT::ValTable &ps = it->second->Values ();
    TRAVERSE_CONST_MAP (ArgsT::ValTable, ps, 
        entity->AddProperty (I_KEY, I_VALUE)) 
    return true;
}

void PropertyTemplate::Update (ArgsT *args, const ArgsT &other)
{
    // assume the right behavior for `operator=`
    *args = other;
}

