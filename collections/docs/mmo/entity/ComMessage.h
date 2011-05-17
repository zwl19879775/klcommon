///
/// @file ComMessage.h
/// @author Kevin Lynx
///
#ifndef ___COM_MESSAGE_H_
#define ___COM_MESSAGE_H_

#include <string>
#include "EntityBase.h"

/// A Com(ponent) message actually is a wrapper for *function call and arguments passing*. 
/// When you want to communicate with some components, the recommended way is to use this 
/// class. Do not be confused about the `Com` prefix with MS (god damn) COM.
class ComMessage 
{
public:
    /// A message name is also the component name. When you passed this message to an entity,
    /// the entity will find the correct component by the message name.
    ComMessage (const std::string &comname) : m_name (comname)
    {
    }

    /// Append an argument, if the argument does not exist, add a new one of course.
    void Set (const std::string &name, const GValue &val)
    {
        m_args.Set (name, val);
    }

    /// Get an argument value by its name. If the argument does not exist, return a nil value.
    GValue Get (const std::string &name) const
    {
        return m_args.Get (name);
    }

    const ParamTable &Args () const { return m_args; }

    const std::string &Name () const { return m_name; }

private:
    std::string m_name;
    ArgsT m_args;
};

#endif

