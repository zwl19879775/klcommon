///
/// @file MessageSender.h
/// @author Kevin Lynx
///
#ifndef ___MESSAGE_SENDER_H_
#define ___MESSAGE_SENDER_H_

#include "../IComponent.h"

// test 
typedef long Message;

/// Implement message communicate with game clients.
/// Actually in a pure component-entity environment, all concrete function can be only
/// implemented by components. So, this is why to have this class.
/// *This component is not very necesary, because Message wrap these operations already*
class MessageSender : public IComponent
{
public:
    MessageSender (Entity *entity);

    void SendMessage (const Message &msg);

    void SendMessageAround (const Message &msg);
};

#endif

