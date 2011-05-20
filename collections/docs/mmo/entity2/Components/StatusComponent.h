///
/// @file StatusComponent.h
/// @author Kevin Lynx
///
#ifndef ___STATUS_COMPONENT_H_
#define ___STATUS_COMPONENT_H_

#include "StatusComponent.h"

class StatusComponent : public StoreComponent
{
public:
    StatusComponent (Entity *entity) :
        StoreComponent (entity, "StatusComponent")
    {
    }
    
    virtual ~StatusComponent () { }

    virtual void OnAdd ();

    virtual void OnRemove ();

    virtual void Reset ();

private:
    bool m_died;
    bool m_move;
    bool m_inFight;
    int m_control;
    int m_chgEquip;
    int m_canMove;
    int m_useSkill;
    int m_skillID; 
    int m_canSel;
};

#endif

