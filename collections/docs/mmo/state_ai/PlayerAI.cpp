///
/// @file PlayerAI.cpp
/// @author Kevin Lynx
///
#include "stdafx.h"
#include "AIDriver.h"
#include "EventImpl/Event.h"
#include "PlayerAI.h"
#include "StateImpl/StateImplDef.h"
#include "StateSet.h"
#include "../Player.h"

#include "../../../../Public/Setup/GlobalSetup.h"

namespace StateAI
{
	PlayerAI::PlayerAI(CPlayer *owner) : BaseAI(owner)
	{
		m_InSkillActTime = false;
		// init state set
		ImplConfig impl;
        impl.AddImpl(ST_PEACE, PSI_PLAYER);
        impl.AddImpl(ST_FIGHT, FSI_PLAYER);
        impl.AddImpl(ST_AGONAL,ASI_PLAYER);
        impl.AddImpl(ST_DEAD, DSI_PLAYER);
		Init(impl, ST_PEACE);
		LogInfo(AI_MODULE, "Initialize PlayerAI(Player id = %s)",
			owner->GetExID().tostring());
	}

	void PlayerAI::SetReturnPeaceTimer(bool useSkill)
	{
		if(!useSkill && !IsInSkillActTime())
		{
			Resume(GlobalSetup::GetSetup()->dwFightStateTime);
		}
		else
		{
			SetInSkillActTime(true);
		}
	}

	void PlayerAI::Attack(long actID, long x, long y, CMoveShape *target)
	{
		if(IsInSkillActTime()/*m_AIDriver->IsRunning()*/)
		{
			// the player is doing some AI stuff
			stModuParam *curSkill = m_Owner->GetInstanceSkill();
			if(curSkill != NULL)
			{
				CNewSkillFactory::RealeaseBaseMoudule(curSkill);
			}
			CMessage msg(MSG_S2C_SKILL_USE);
			msg.Add((uchar)SKILL_USE_RESULT_FAILED);
			msg.Add(m_Owner->GetExID());
			msg.Add((long) curSkill->GetID());
			msg.Add((uchar)1);
			msg.Add((ulong)0);
			msg.Add((uchar)SKILL_USE_FAILED_INVALID_UNKNOWN);
			msg.SendToPlayer(m_Owner->GetExID());
			return ;
		}

		if(!m_Owner->GetForceAttack() && HasTarget())
		{
			CMoveShape *curTarget = GetTarget();
			if(curTarget != NULL)
			{
				target = curTarget;
			}
		}

		SetTarget(target->GetExID(), target->GetType());

		stModuParam *curSkill = m_Owner->GetInstanceSkill();
		if(curSkill != NULL)
		{
            if(!BeginSkill(x, y, target))
			{
				LogError(AI_MODULE, "BeginSkill() error...");
				return ;
			}

			int skillType = curSkill->GetAddSub();
			if(skillType == eSub_Type)
			{
				if(CurState()->Type() == ST_PEACE)
				{
					LogInfo(AI_MODULE, "PlayerAI(Owner Id=%s) Changed its state from %s to %s on event %d.",
						m_Owner->GetExID().tostring(),
						State2Str(ST_PEACE),
						State2Str(ST_FIGHT),
						UseSkillEvent().Type()
);
					ChangeState(ST_FIGHT, UseSkillEvent());
				}
				else
				{
					SetInSkillActTime(true);
				}
			}
		}
	}

    bool PlayerAI::BeginSkill(long x, long y, CMoveShape *target)
    {
		stModuParam *curSkill = m_Owner->GetInstanceSkill();
		if(curSkill == NULL)
		{
			LogError(AI_MODULE, "get skill instance error.");
            return false;
        }
        if(BaseAI::BeginSkill(curSkill, x, y, target))
        {
            if(curSkill->GetID() < 50000)
            {
                ((CPlayer*) m_Owner)->OnWeaponDamaged();
            }
            /// 技能使用成功，更新杂念值。
            //((CPlayer*)m_Owner)->UseSkillUpdateBuddhaThoughts();
            return true;
        }
        return false;
    }

	void PlayerAI::OnEvent(const AIEvent &event)
	{
		LogInfo(AI_MODULE, "player(id=%s) recv ai event:%s",
			GetOwner()->GetExID().tostring(),
			BaseAI::Event2Str(event.Type())
);
		switch(event.Type())
		{
		case ET_KILL:
			LogInfo(AI_MODULE, "PlayerAI(Owner Id=%s) Changed its state from %s to %s on event %d",
				m_Owner->GetExID().tostring(),
				State2Str(CurStateType()),
				State2Str(ST_DEAD),
				ET_KILL
);
			ChangeState(ST_DEAD);
			break;
		}
	}
}
