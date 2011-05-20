///
/// @file MoveComponent.h
/// @author Kevin Lynx
///
#ifndef ___MOVE_COMPONENT_H_
#define ___MOVE_COMPONENT_H_

#include "StoreComponent.h"

class MoveComponent : public StoreComponent
{
public:
    MoveComponent (Entity *entity) :
        StoreComponent (entity, "MoveComponent")
    {
    }
    
    virtual ~MoveComponent () { }

    virtual void OnAdd ();

    virtual void OnRemove ();

    virtual void Reset ();

private:
};

#endif

