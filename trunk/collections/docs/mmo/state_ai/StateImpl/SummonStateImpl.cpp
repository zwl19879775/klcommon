#include "stdafx.h"
#include "SummonStateImpl.h"
#include "../BaseAI.h"
#include "../MonsterAI.h"
#include "../../Monster.h"
#include "../../BuffSkill/SummVine.h"
#include "../../BuffSkill/SummonMan.h"
#include "../../../../../Public/Setup/GlobalSetup.h"

namespace  StateAI
{
    IMPL_LIFE_POLICY_FUNC(SummonPeaceState, AIState);

    bool SummonPeaceState::SearchEnemy(BaseType::EntityType *entity)
    {
		if (entity == NULL)
		{
			LogError(AI_MODULE, "entity object is null.");
			return false;
		}

        AIState *searchAction = entity->GetState(ST_SEARCH);
		if (searchAction == NULL)
		{
			LogError(AI_MODULE, "searchAction is null.");
		}
        assert(searchAction);
        searchAction->Execute(entity);
        if(entity->HasTarget())
        {
            entity->ChangeState(ST_FIGHT);
            entity->Resume(0);
            return true;
        }
        return false;
    }

    void SummonPeaceState::ReceiveEvent(BaseType::EntityType *entity, const BaseType::EventType &ev)
    {
		if (entity == NULL)
		{
			LogError(AI_MODULE, "entity object is null.");
			return;
		}
        if(ev.Type() == ET_HURT)
        {
            entity->ChangeState(ST_FIGHT);
            AIState *search_action = entity->GetState(ST_SEARCH);
			if (search_action == NULL)
			{
				LogError(AI_MODULE, "search_action is null.");
			}
            assert(search_action);
            search_action->ReceiveEvent(entity, ev);
        }
        else if(ev.Type() == ET_KILL)
        {
            entity->ChangeState(ST_DEAD);
        }
    }

    void SummonPeaceState::Execute(BaseType::EntityType *entity)
    {
		if (entity == NULL)
		{
			LogError(AI_MODULE, "entity is null.");
			return;
		}
        if(SearchEnemy(entity))
        {
            return;
        }

        /// Stand ... Don't move
        MonsterAI *ai = static_cast<MonsterAI*>(entity);
        CMonster  *monster = static_cast<CMonster*>(entity->GetOwner());
		if (monster == NULL)
		{
			LogError(AI_MODULE, "MonsterAI do not have owner.");
		}
        assert(ai && monster);
        ai->Stand(monster->GetStopFrame());
    }


    IMPL_LIFE_POLICY_FUNC(SummonDeadState, AIState);

    void SummonDeadState::Enter(BaseType::EntityType *entity, const AITransition &tran)
    {
		if (entity == NULL)
		{
			LogError(AI_MODULE, "entity object is null.");
			return;
		}
        CMonster *monster = static_cast<CMonster*>(entity->GetOwner());
        entity->Stop();
        entity->Resume(GlobalSetup::GetSetup()->dwMonsterKeepDeadTime);

        SKILL::SummVine *vine = dynamic_cast<SKILL::SummVine*>(monster->GetSummmon());
		if (vine == NULL)
		{
			LogError(AI_MODULE, "monster->GetSummon() error: vine is null.");
		}
        assert(vine);
        if (vine != NULL)
        {
            //GetInst(SKILL::SummonMan).RemoveSummon(vine);
        }
    }

    void SummonDeadState::Execute(BaseType::EntityType *entity)
    {
		if (entity == NULL)
		{
			LogError(AI_MODULE, "entity object is null.");
			return;
		}
        CMonster *monster = static_cast<CMonster*>(entity->GetOwner());
        monster->AddDelEvent(0);
    }

}
