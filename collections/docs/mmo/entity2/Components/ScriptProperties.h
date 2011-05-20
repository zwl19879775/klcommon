///
/// @file ScriptProperties.h
/// @author Kevin Lynx
///
#ifndef ___SCRIPT_PROPERTIES_H_
#define ___SCRIPT_PROPERTIES_H_

#include <map>
#include <string>
#include "../IComponent.h"

/// ScriptProperties is used to manage some properties which only accessed in 
/// script. Do NOT add properties dynamically. When this class has been created,
/// the property count has been decided.
class ScriptProperties : public IComponent
{
public:
    /// Use table to store all properties.
    typedef std::map<std::string, GValue> PropertyTable;
public:
    ScriptProperties (Entity *entity);

    virtual ~ScriptProperties ();

    virtual void OnAdd ();

    virtual void OnRemove ();

    virtual GValue GetPropertyVal (const std::string &name) const;

    virtual bool SetPropertyVal (const std::string &name, const GValue &val);

    virtual bool IncPropertyVal (const std::string &name, double inc);
private:
    /// Initialize the property table.
    bool InitProperties ();
private:
    PropertyTable m_properties;
};

#endif

