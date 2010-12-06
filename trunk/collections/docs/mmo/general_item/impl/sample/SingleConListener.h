///
/// @file SingleConListener.h
///
///
#ifndef ___SINGLECONLISTENER_H_
#define ___SINGLECONLISTENER_H_

#include "../GIContainerListener.h"

class ObjOperSender;
class CPlayer;

class SingleConListener : public GI::ContainerListener
{
public:
    SingleConListener();

    virtual ~SingleConListener();

    void Begin( CPlayer *owner, ObjOperSender *res, long type );

    void End();

    virtual void OnAdd( GI::BaseContainer *con, const GI::Object *obj );

    virtual void OnRemove( GI::BaseContainer *con, const GI::Object *obj );

    virtual void OnSetAmount( GI::BaseContainer *con, const GI::Object *obj );
private:
    CPlayer *m_owner;
    ObjOperSender *m_res;
    long m_type;
};

#endif

