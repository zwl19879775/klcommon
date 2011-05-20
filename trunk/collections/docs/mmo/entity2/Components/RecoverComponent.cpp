///
/// @file RecoverComponent.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "RecoverComponent.h"
#include "TimerComponent.h"
#include "../Entity.h"

#define NULL_RET ;
#define CHECK_TIMER(var, entity, ret) \
    TimerComponent *var = entity->GetComponent<TimerComponent> ("TimerComponent"); \
    if (var == NULL) { \
        ELogError ("Not found the TimerComponent in entity"); \
        return ret; \
    }

RecoverComponent::RecoverComponent (Entity *entity) :
    IComponent (entity, "RecoverComponent")
{
}

RecoverComponent::~RecoverComponent ()
{
    StopAll ();
}

bool RecoverComponent::Start (const std::string &name, const std::string &maxname, int inter, int inc)
{
    RecoverTable::iterator it = m_recovers.find (name);
    if (it != m_recovers.end ())
    {
        ELogWarn ("The recover (%s) already exists", name.c_str ());
        return false;
    }
    CHECK_TIMER (timer, m_entity, false);
    Recover recover;
    recover.name = name;
    recover.maxname = maxname;
    recover.inter = inter;
    recover.inc = inc;
    recover.tag = CreateStringArg (name);
    if (timer->Register (name, this, &RecoverComponent::OnRecover, recover.tag, inter, inter))
    {
        ELogDebug ("Add a new recover (%s)", name.c_str ());
        m_recovers[name] = recover;
        return true;
    }
    DestroyStringArg (recover.tag);
    ELogWarn ("Register a timer for recover (%s) failed", name.c_str ());
    return false;
}

void RecoverComponent::Stop (const std::string &name)
{
    RecoverTable::iterator it = m_recovers.find (name);
    if (it == m_recovers.end ())
    {
        ELogWarn ("Not found the recover (%s)", name.c_str ());
        return ;
    }
    CHECK_TIMER (timer, m_entity, NULL_RET);
    timer->UnRegister (name);
    DestroyStringArg (it->second.tag);
    m_recovers.erase (name);
}

void RecoverComponent::StopAll ()
{
    CHECK_TIMER (timer, m_entity, NULL_RET);
    TRAVERSE_MAP (RecoverTable, m_recovers, 
        timer->UnRegister (I_KEY); DestroyStringArg (I_VALUE.tag));
    m_recovers.clear ();
}

void RecoverComponent::OnRecover (void *arg)
{
    const char *name = StringArg (arg);
    RecoverTable::iterator it = m_recovers.find (name);
    if (it == m_recovers.end ())
    {
        ELogWarn ("Not found the recover (%s)", name.c_str ());
        return ;
    }
    const Recover &recover = it->second;
    const GValue &max = m_entity->GetSharedPropertyVal (recover.maxname);
    if (VALUE_NIL (max))
    {
        ELogWarn ("Recover max value (%s) is nil", recover.maxname.c_str ());
        Stop (name);
        return ;
    }
    const GValue &cur = m_entity->GetSharedPropertyVal (recover.name);
    if (VALUE_NIL (cur))
    {
        ELogWarn ("Recover value (%s) is nil", recover.name.c_str ());
        Stop (name);
        return ;
    }
    double ret = cur.GetNumber () + recover.inc;
    if (ret >= max.GetNumber ())
    {
        // Done
        ELogDebug ("Recover (%s) DONE", name);
        m_entity->SetSharedPropertyVal (recover.name, max);
        Stop (name);
        return ;
    }
    m_entity->IncSharedPropertyVal (recover.name, recover.inc);
}

