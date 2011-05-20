///
/// @file MessageSender.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "MessageSender.h"
#include "../Entity.h"

MessageSender::MessageSender (Entity *entity) :
    IComponent (entity, "MessageSender")
{
}

// currently, it depends on `Player` object.
void MessageSender::SendMessage (const Message &msg)
{
    // TODO:
    // msg.SendToPlayer (m_entity->ID ());
}

