///
/// @file SocialComponent.h
/// @author Kevin Lynx
///
#ifndef ___SOICAL_COMPONENT_H_
#define ___SOICAL_COMPONENT_H_

#include "StoreComponent.h"

class SocialComponent : public StoreComponent
{
public:
    SocialComponent (Entity *entity) :
        StoreComponent (entity, "SocialComponent")
    {
    }
    
    virtual ~SocialComponent () { }

    virtual void OnAdd ();

    virtual void OnRemove ();

    virtual void Reset ();

private:
    CGUID m_guildID;
    int m_guildPos;
    int m_faction;
    CGUID m_teamID;
    int m_teamPos;
};

#endif

