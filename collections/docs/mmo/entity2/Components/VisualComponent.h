///
/// @file VisualComponent.h
/// @author Kevin Lynx
///
#ifndef ___VISUAL_COMPONENT_H_
#define ___VISUAL_COMPONENT_H_

#include "StoreComponent.h"

class VisualComponent : public StoreComponent
{
public:
    VisualComponent (Entity *entity) :
        StoreComponent (entity, "VisualComponent")
    {
    }
    
    virtual ~VisualComponent () { }

    virtual void OnAdd ();

    virtual void OnRemove ();

    virtual void Reset ();

private:
    int m_ride;
    long m_body;
    long m_hairStyle;
    long m_hairColor;
    long m_jewelry;
    long m_face;
    long m_size;
    long m_ui;
};

#endif

