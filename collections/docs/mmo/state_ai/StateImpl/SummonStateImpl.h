///
/// @brief about Summon state type
///

#pragma  once

#include "../AIStateDefs.h"

namespace StateAI
{
    /// ºÍÆ½×´Ì¬
    class  SummonPeaceState : public AIState
    {
    public:
        SummonPeaceState(MachineType *machine) : AIState(machine , ST_PEACE)
        {

        }

        void ReceiveEvent(BaseType::EntityType *entity, const BaseType::EventType &ev);

        void Execute(BaseType::EntityType *entity);

        DECL_LIFE_POLICY_FUNC(SummonPeaceState, AIState);

    private:

        bool SearchEnemy(BaseType::EntityType *entity);

    };

    /// ËÀÍö×´Ì¬
    class  SummonDeadState  : public AIState
    {
    public:
        SummonDeadState(MachineType *machine) : AIState(machine, ST_DEAD)
        {
        }

        void Enter(EntityType *entity, const TransitionType &);

        void Execute(BaseType::EntityType *entity);

        DECL_LIFE_POLICY_FUNC(SummonDeadState, AIState);
    };
}
