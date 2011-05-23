///
/// @file EncodeComponent.h
/// @author Kevin Lynx
/// 
#ifndef ___ENCODE_COMPONENT_H_
#define ___ENCODE_COMPONENT_H_

#include "../IComponent.h"

class EncodeComponent : public IComponent
{
public:
    EncodeComponent (Entity *entity);

    virtual ~EncodeComponent ();

    void EncodeDetail (DBWriteSet &db);

    void EncodeGeneral (DBWriteSet &db);

    void EncodeCommon (DBWriteSet &db);
};

#endif

