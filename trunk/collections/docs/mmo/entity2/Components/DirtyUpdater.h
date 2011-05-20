///
/// @file DirtyUpdater.h
/// @author Kevin Lynx
///
#ifndef ___DIRTY_UPDATER_H_
#define ___DIRTY_UPDATER_H_

#include "../IComponent.h"
#include <list>
#include <string>

/// DirtyUpdate implements a dirty property functionality. And it's a manual
/// updater. Than means it's your responsibility to mark dirty and update.
class DirtyUpdater : public IComponent
{
public:
    typedef std::list<std::string> DirtyList;
public:
    DirtyUpdater (Entity *entity);

    /// Mark a property dirty. A dirty property will be updated to game client.
    void Mark (const std::string &name);

    /// Update all dirty properties to game client. If 'now' flag is false, it 
    /// will register a timer and update later.
    void Update (bool now = true);

private:
    DirtyList m_dirties;
};

#endif

