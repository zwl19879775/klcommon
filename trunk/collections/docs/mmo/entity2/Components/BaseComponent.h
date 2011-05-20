///
/// @file BaseComponent.h
/// @author Kevin Lynx
///
#ifndef ___BASE_COMPONENT_H_
#define ___BASE_COMPONENT_H_

#include "StoreComponent.h"

class BaseComponent : public StoreComponent
{
public:
    BaseComponent (Entity *entity) :
        StoreComponent (entity, "BaseComponent")
    {
    }
    
    virtual ~BaseComponent () { }

    virtual void OnAdd ();

    virtual void OnRemove ();

    virtual void Reset ();

private:
};

#endif

