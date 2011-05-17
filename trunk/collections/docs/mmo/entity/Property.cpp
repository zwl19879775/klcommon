///
/// @file Property.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include <algorithm>
#include "Property.h"
#include "IComponent.h"

void Property::AddObserver (IComponent *com)
{
    ObserverList::iterator it = std::find (m_observers.begin(), m_observers.end(), com);
    if (it != m_observers.end() )
    {
        ELogWarn ("The observer (%s) already exist, ignore it", com->Name ().c_str ());
        return ;
    }
    m_observers.push_back (com);
}

void Property::Set (const GValue &val)
{
    if (m_observers.size () == 0)
    {
        m_val = val;
    }
    else
    {
        ELogDebug ("Notify all observers for property (%s)", m_name.c_str ());
        GValue old = m_val;
        m_val = val;
        ArgsT args;
        args.Set ("old-val", old);
        TRAVERSE_LIST (ObserverList, m_observers, 
            L_VALUE->OnNotify (*this, &args))
    }
}

