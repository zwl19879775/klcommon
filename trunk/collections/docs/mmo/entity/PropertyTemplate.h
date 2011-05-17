///
/// @file PropertyTemplate.h
/// @author Kevin Lynx
///
#ifndef ___PROPERTY_TEMPLATE_H_
#define ___PROPERTY_TEMPLATE_H_

#include <map>
#include "EntityBase.h"

class Entity;

/// Represents a property table template. In a game, there're many properties table
/// associated with some kind of object. And this class map a object type to a config
/// property template.
class PropertyTemplate
{
public:
    /// <object-type, property-table>
    typedef std::map<int, ArgsT*> TemplateTable;
public:
    PropertyTemplate () {}

    /// Add a property template. If the template exists, update it.
    void Add (int type, const ArgsT &pt);

    /// Append all properties in the template to the corresponding entity by type.
    /// If not found the corresponding template, return false.
    bool AddToEntity (Entity *entity) const;
private:
    /// Update `args` to `other`.
    void Update (ArgsT *args, const ArgsT &other);
private:
    TemplateTable m_ptable;
};

#endif

