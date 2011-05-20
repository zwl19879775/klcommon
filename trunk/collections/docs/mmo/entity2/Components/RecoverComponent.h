///
/// @file RecoverComponent.h
/// @author Kevin Lynx
///
#ifndef ___RECOVER_COMPONENT_H_
#define ___RECOVER_COMPONENT_H_

#include "../IComponent.h"
#include <map>
#include <string>

/// RecoverComponent can increase some specified properties value by timer. It will manage
/// a list of properties. Note, this component assumes these property value as int type.
class RecoverComponent : public IComponent
{
public:
    struct Recover
    {
        std::string name;
        std::string maxname;
        int inter;
        int inc;
        void *tag;
    };
    typedef std::map<std::string, Recover> RecoverTable;
public:
    RecoverComponent (Entity *entity);

    /// Stop all these increase operations.
    virtual ~RecoverComponent ();

    /// Start to recover a property specified by `name` value. The property name will also be
    /// the `Recover` id name. 
    bool Start (const std::string &name, const std::string &maxname, int inter, int inc);

    /// Stop a recover by its name, the name usually is the property name.
    /// NOTE: when a property has been recovered to the max value, the process will stop auto.
    void Stop (const std::string &name);

    void StopAll ();

    /// Called by the timer component.
    void OnRecover (void *arg);

private:
    RecoverTable m_recovers;
};

#endif

