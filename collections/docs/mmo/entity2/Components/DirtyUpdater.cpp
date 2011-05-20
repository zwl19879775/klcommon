///
/// @file DirtyUpdater.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "DirtyUpdater.h"
#include "../Entity.h"

DirtyUpdater::DirtyUpdater (Entity *entity) :
    IComponent (entity, "DirtyUpdater")
{
}

void DirtyUpdater::Mark (const std::string &name)
{
    // even if there's already an item with the same name
    m_dirties.push_back (name);
}

void DirtyUpdater::Update (bool now)
{
    if (!now)
    {
        ELogInfo ("Register a timer for DirtyUpdater");
        // TODO:register a timer
        return;
    }
    // use a message sender component to send messages
    ELogDebug ("Send dirty properties update message"); 
    // TODO: encode the dirty properties into a message
}

