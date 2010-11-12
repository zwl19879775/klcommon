///
/// @file PlayerConListener.h
///
///
#ifndef ___PLAYERCONLISTENER_H_
#define ___PLAYERCONLISTENER_H_

#include "../GIContainerListener.h"

class PlayerContainer;
class ObjOperSender;

class PlayerConListener : public GI::ContainerListener
{
public:
    PlayerConListener();

    virtual ~PlayerConListener();
    
    void Begin( PlayerContainer *con, ObjOperSender *res );

    void End();

    virtual void OnAdd( GI::BaseContainer *con, const GI::Object *obj );

    virtual void OnRemove( GI::BaseContainer *con, const GI::Object *obj );

    virtual void OnMoved( GI::BaseContainer *srcCon, GI::BaseContainer *destCon, 
            const GI::Object *obj );
private:
    PlayerContainer *m_con;
    ObjOperSender *m_res;
};

#endif

