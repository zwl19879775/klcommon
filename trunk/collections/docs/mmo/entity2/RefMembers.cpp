///
/// @file RefMember.cpp
/// @author Kevin Lynx
/// 
#include "RefMembers.h"

RefMembers::~RefMembers ()
{
    for (MemberTable::iterator it = m_mems.begin(); it != m_mems.end(); ++ it)
    {
        delete it->second;
    }
}

void RefMembers::Remove (const std::string &name)
{
    MemberTable::iterator it = m_mems.find (name);
    if (it != m_mems.end ())
    {
        delete it->second;
        m_mems.erase (it);
    }
}

bool RefMembers::SetValue (const std::string &name, const GValue &val)
{
    MemberTable::iterator it = m_mems.find (name);
    if (it == m_mems.end ())
    {
        return false;
    }
    it->second->Set (val);
    return true;
}

bool RefMembers::IncValue (const std::string &name, double inc)
{
    MemberTable::iterator it = m_mems.find (name);
    if (it == m_mems.end ())
    {
        return false;
    }
    it->second->Inc (inc);
    return true;
}

GValue RefMembers::GetValue (const std::string &name)
{
    MemberTable::iterator it = m_mems.find (name);
    if (it == m_mems.end ())
    {
        return GValue ();
    }
    return it->second->Get ();
}

