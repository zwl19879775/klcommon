///
/// @file Property.h
/// @author Kevin Lynx
///
#ifndef ___PROPERTY_H_
#define ___PROPERTY_H_

#include <string>
#include <list>
#include "EntityBase.h"

class IComponent;

/// A simple property class, only has a name, a value, and a list of observers.
/// Currently the observers will be notified only when the value changed.
class Property 
{
public:
    typedef std::list<IComponent*> ObserverList;
public:
    Property (const std::string &name, const GValue &val = GValue ()) : 
        m_name (name), m_val (val)
    {
    }

    /// Add an observer, if the observer already exists, ignore it.
    void AddObserver (IComponent *com);

    /// Set the property value. It will notify these observers by `OnNotify`.
    void Set (const GValue &val);

    /// Get the property value.
    const GValue &Get () const { return m_val; }

    const std::string &Name () const { return m_name; }

private:
    std::string m_name;
    GValue m_val;
    ObserverList m_observers;
};

#endif

