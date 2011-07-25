///
/// @file QuickMoveState.cpp
/// @author Kevin Lynx
///
#include "stdafx.h"
#include "QuickMoveState.h"
#include "../BaseAI.h"
#include "../EventImpl/Event.h"
#include "../../Monster.h"

namespace StateAI
{
	IMPL_LIFE_POLICY_FUNC (QuickMoveState, AIState);

	void QuickMoveState::Enter (EntityType *entity, const TransitionType &tran)
	{
        assert (tran.from);
        m_lastState = tran.from->Type ();
        const EventTransition &evtran = static_cast<const EventTransition&> (tran);
        const QuickMoveEvent &ent = static_cast<const QuickMoveEvent&> (evtran.event);
        m_destX = ent.m_destX;
        m_destY = ent.m_destY;
        m_speed = ent.m_speed;
        CMonster *monster = dynamic_cast<CMonster*> (entity->GetOwner ());
        m_lastSpeed = monster->GetSpeed ();
        monster->SetSpeed (m_speed);
        LogTrace (AI_MODULE, "Start QuickMoveState (%d, %d) (%f)", m_destX, m_destY, m_speed);
	}

	void QuickMoveState::Execute (BaseType::EntityType *entity)
	{
        CMonster *monster = dynamic_cast<CMonster*> (entity->GetOwner ());
        if (monster->GetTileX () == m_destX && monster->GetTileY () == m_destY)
        {
            Done (entity);
            return ;
        }
        entity->Move (m_destX, m_destY);
	}

    void QuickMoveState::ReceiveEvent (BaseType::EntityType *entity, const BaseType::EventType &ev)
    {
        if (ev.Type () == ET_QUICKMOVESTOP)
        {
            LogTrace (AI_MODULE, "Receive QuickMoveStop event.");
            Done (entity);
        }
    }

    void QuickMoveState::Reset ()
    {
        m_destX = 0;
        m_destY = 0;
        m_speed = 0;
        m_lastSpeed = 0;
        m_lastState = ST_INVALID;
    }

    void QuickMoveState::Done (EntityType *entity)
    {
        LogTrace (AI_MODULE, "QuickMoveState execute done, return to state (%d)", m_lastState);
        entity->ChangeState (m_lastState);
        CMonster *monster = dynamic_cast<CMonster*> (entity->GetOwner ());
        monster->SetSpeed (m_lastSpeed);
        Reset ();
    }
}
