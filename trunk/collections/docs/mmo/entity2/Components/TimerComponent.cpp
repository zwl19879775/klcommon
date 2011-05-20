///
/// @file TimerComponent.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "TimerComponent.h"

TimerComponent::~TimerComponent ()
{
    if (m_timers.size () > 0)
    {
        ELogWarn ("Already has some timers (%d)", (int) m_timers.size ());
        UnRegisterAll ();
    }
}

void TimerComponent::UnRegister (const std::string &name)
{
    TimerTable::iterator it = m_timers.find (name);
    if (it == m_timers.end ())
    {
        ELogWarn ("Not found the timer (%s)", name.c_str ());
        return;
    }
    ELogDebug ("UnRegister a timer (%s)", name.c_str ());
    // TODO: unregister it by its id
    m_timers.erase (name);
}

void TimerComponent::UnRegisterAll ()
{
    ELogDebug ("UnRegister all timers");
    // TODO: unregister it by its id
    TRAVERSE_MAP (TimerTable, m_timers, (void)0);
    m_timers.clear ();
}

void TimerComponent::DoCallback (const std::string &name)
{
    TimerTable::const_iterator it = m_timers.find (name);
    if (it == m_timers.end ())
    {
        ELogWarn ("No callback (%s) in the timer table", name.c_str ());
        return ;
    }
    const Timer &t = it->second;
    // pass t.obj as *this* pointer
    t.fn (t.obj, t.arg);
}

bool TimerComponent::DoRegister (const std::string &name, IComponent *obj, Callback fn, void *arg,
        int first, int inter)
{
    TimerTable::iterator it = m_timers.find (name);
    if (it != m_timers.end ())
    {
        ELogWarn ("The timer (%s) already exists", name.c_str ());
        return false;
    }
    Timer t;
    t.arg = arg;
    t.obj = obj;
    t.fn = fn;
    t.id = 0; /*TODO: register by GameEvent, the `name` as the timer argument*/
    m_timers[name] = t;
    ELogDebug ("Registered a timer (%s)", name.c_str ());
    return true;
}

