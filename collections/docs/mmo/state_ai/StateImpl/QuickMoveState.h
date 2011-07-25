///
/// @file QuickMoveState.h
/// @author Kevin Lynx
///
#ifndef ___QUICK_MOVE_STATE_H_
#define ___QUICK_MOVE_STATE_H_

#include "../AIStateDefs.h"

namespace StateAI
{
    /// QuickMoveState is an AI state, which only move the object quickly to dest position.
    class QuickMoveState : public AIState
    {
    public:
        QuickMoveState (MachineType *machine) : AIState (machine, ST_QUICKMOVE)
        {
            Reset ();
        }

        void Reset ();

        void Enter (EntityType *entity, const TransitionType &);

		void ReceiveEvent (BaseType::EntityType *entity, const BaseType::EventType &ev);

		void Execute (BaseType::EntityType *entity);

		DECL_LIFE_POLICY_FUNC (QuickMoveState, AIState);
    private:
        void Done (EntityType *entity);
    private:
        long m_destX;
        long m_destY;
        float m_speed;
        float m_lastSpeed;
        long m_lastState;
    };
}

#endif
