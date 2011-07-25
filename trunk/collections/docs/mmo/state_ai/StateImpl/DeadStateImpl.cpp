///
/// @file DeadStateImpl.cpp
///
#include "stdafx.h"
#include "DeadStateImpl.h"
#include "../BaseAI.h"
#include "../../Monster.h"

#include "../../../../../Public/Setup/GlobalSetup.h"

namespace StateAI
{
	IMPL_LIFE_POLICY_FUNC(DeadStateNormal, AIState);

	void DeadStateNormal::Enter(EntityType *entity, const TransitionType &)
	{
		if (entity == NULL)
		{
			LogError(AI_MODULE, "entity is null.");
			return;
		}
		CMonster *monster = static_cast<CMonster*>(entity->GetOwner());
        entity->Stop();
		if(monster->GetCollectionID() == 0)
		{
			entity->Resume(GlobalSetup::GetSetup()->dwMonsterKeepDeadTime);
		}
		else
		{
			entity->Resume(GlobalSetup::GetSetup()->dwCanCollectMonsterKeepDeadTime);
		}
		LogDebug(AI_MODULE, "monster=%s", monster->GetExID().tostring());
	}

	void DeadStateNormal::Execute(BaseType::EntityType *entity)
	{
		if (entity == NULL)
		{
			LogError(AI_MODULE, "entity is null.");
			return;
		}
		CMonster *monster = static_cast<CMonster*>(entity->GetOwner());
		monster->AddDelEvent(0);
	}
}
