///
/// @file PetAI.cpp
///
#include "stdafx.h"
#include "PetAI.h"
#include "AIDriver.h"
#include "EventImpl/Event.h"
#include "../Pet.h"
#include "../PetCtrl.h"
#include "../../../../Public/Setup/GlobalSetup.h"

namespace StateAI
{
    PetAI::PetAI(CMoveShape *owner) : BaseAI(owner)
    {
		LogInfo(AI_MODULE, "Create PetAI object, its owner\'s id is %s",
			owner->GetExID().tostring());
        m_Host = NULL;
    }

    PetAI::~PetAI()
    {
        LogoutRetPeaceAct();
    }

    void PetAI::Run(long timerID, long timerType)
    {
        //m_AIDriver->OnTimeOut(timerType);
        //if(m_Owner == NULL ||
        //    m_Host == NULL ||
        //    !((CPet*)m_Owner)->IsActive())
        //{
        //    m_Owner->OnCannotMove();
        //    return;
        //}

        //if(timerType == AIDriver::AI_EVENT)
        //{
        //    long state = m_Owner->GetState();
        //    if(state == CShape::STATE_DIED)
        //    {
        //        OnDel();
        //    }
        //    else if(state == CShape::STATE_FIGHT ||
        //        state == CShape::STATE_PEACE)
        //    {
        //        CPet *pet = (CPet*) m_Owner;
        //        if(pet->GetFightType() == PET_FIGHETYPE_FIGHT)
        //        {
        //            SearchEnemy();
        //        }
        //    }
        //}
        //else if(timerType == AIDriver::RETPEACE_EVENT)
        //{
        //    OnReturnPeace();
        //}
    }

    void PetAI::SetTarget(const CGUID &targetID, long targetType)
    {
        BaseAI::SetTarget(targetID, targetType);
        CPet *pet = (CPet*) GetOwner();
        //if(pet->GetPetType() != PET_TYPE_TRAP)
        {
            GetInst(CPetCtrl).InformClientAI(pet, PET_OT_AI_SET_TARGET);
        }
    }

    void PetAI::SetHost(CMoveShape *host)
    {
		LogInfo(AI_MODULE, "set host for PetAI(host=%s)",
			host->GetExID().tostring());
        m_Host = host;
    }

    bool PetAI::OnMove(float destX, float destY)
    {
        if(!BaseAI::OnMove(destX, destY))
        {
            return false;
        }
        CPet *pet = (CPet*) GetOwner();
        // add ai event to search enemy when moving
        if(pet->GetFightType() == PET_FIGHETYPE_FIGHT && !HasTarget())
        {
            Resume(CONTINUE_TIME);
        }
        return true;
    }

    void PetAI::Attack(long actID, long x, long y, CMoveShape *target)
    {
        if(m_Host == NULL)
        {
			LogError(AI_MODULE, "m_Host is null.");
            return;
        }
		if (target == NULL)
		{
			LogError(AI_MODULE, "target is null.");
			return;
		}
        stModuParam *currentSkill = m_Owner->GetInstanceSkill();
        if(currentSkill != NULL)
        {
            eAddSub skillType = currentSkill->GetAddSub();
            if(BeginSkill(currentSkill, x, y, target) &&
                skillType == eSub_Type)
            {
                if(target != NULL && target != GetTarget())
                {
                    SetTarget(target->GetExID(), target->GetType());
                }
            }
        }
		else
		{
			LogError(AI_MODULE, "m_Owner->GetInstanceSkill() error");
		}
    }

    bool PetAI::SearchEnemy()
    {
        CPet *owner = (CPet*) m_Owner;
        CMoveShape *host = owner->GetHost();
        if(NULL == host)
        {
			LogError(AI_MODULE, "m_Owner->GetHost() error.");
            return false;
        }
        CMoveShape *target = GetTarget();

        // 当前有追踪目标
        if(target != NULL)
        {
            long dis = owner->Distance(target);
            long tracDis = owner->GetTrackRange();
            if(!target->IsDied() &&
                tracDis > dis &&
                !target->IsGod())
            {
                return true;
            }
        }

        // 寻找所在场景
        CServerRegion *region = dynamic_cast<CServerRegion*>(host->GetFather());
        if(NULL == region)
        {
            return false;
        }

        vector<CShape*> vOut;
        region->FindAroundObject(owner, TYPE_PLAYER, vOut);
        region->FindAroundObject(owner, TYPE_MONSTER, vOut);
        region->FindAroundObject(owner, TYPE_PET, vOut);

        bool ret = false;
        map<long, CMoveShape*> AroundObjs;
        long distance = 0;
        for(size_t i=0; i < vOut.size(); ++i)
        {
            target = (CMoveShape*)vOut[i];
            if(target != NULL &&
                target->IsDied() &&
                !target->IsGod() &&
                target->IsAttackAble(owner))
            {
                distance = m_Owner->Distance(target->GetTileX(), target->GetTileY());
                AroundObjs[distance] = target;
                ret = true;
            }
        }

        if(ret)		// 满足条件设置对象
        {
            map<long, CMoveShape*>::iterator itr = AroundObjs.begin();
            target = itr->second;
            SetTarget(target->GetExID(), target->GetType());
            //if(PET_TYPE_TRAP == owner->GetPetType())
            //{
            //    owner->SetActTime(0, 0);		// 避免寻敌后无法攻击
            //    owner->OnTrapSpring();
            //}
        }

        return true;
    }

    void PetAI::AddRetPeaceAct()
    {
        long trigerTime = GlobalSetup::GetSetup()->dwFightStateTime;
        SetTimer(AIDriver::RETPEACE_EVENT, trigerTime);
    }

    void PetAI::LogoutRetPeaceAct()
    {
        StopTimer(AIDriver::RETPEACE_EVENT);
    }

    void PetAI::ChangeStateTo(long state)
    {
        if(m_Owner->GetState() == state ||
            m_Owner->IsDied())
        {
            return;
        }
		LogInfo(AI_MODULE, "PetAI Changed its owner\'s(id=%s) state from %s to %s",
			m_Owner->GetExID().tostring(),
			CShape::State2Str(m_Owner->GetState()),
			CShape::State2Str(state)
);
        m_Owner->ChangeStateTo((CShape::eSTATE) state);
    }

    void PetAI::OnDead()
    {
        Resume(GlobalSetup::GetSetup()->dwMonsterKeepDeadTime);
        m_Owner->OnDied();
    }

    void PetAI::OnDel()
    {
        CPet *pet = (CPet*) m_Owner;
        pet->DelFromRegion(PET_OT_DISAPPEAR);
    }

    void PetAI::OnHurt()
    {
        if(!m_Owner->IsGod())
        {
			LogInfo(AI_MODULE, "PetAI Changed its owner\'s(id=%s) state from %s to %s",
				m_Owner->GetExID().tostring(),
				CShape::State2Str(m_Owner->GetState()),
				CShape::State2Str(CShape::STATE_FIGHT)
);
            m_Owner->ChangeStateTo(CShape::STATE_FIGHT);
        }
    }

    void PetAI::OnSkillEnd(long subType)
    {
        LogoutRetPeaceAct();
        AddRetPeaceAct();
        if(eAdd_Type == subType &&
            CShape::STATE_PEACE == GetOwner()->GetState())
        {
			LogInfo(AI_MODULE, "PetAI Changed its owner\'s(id=%s) state from %s to %s",
				GetOwner()->GetExID().tostring(),
				CShape::State2Str(GetOwner()->GetState()),
				CShape::State2Str(CShape::STATE_PEACE)
);
            GetOwner()->ChangeStateTo(CShape::STATE_PEACE);

        }
        CMoveShape *target = GetTarget();
        if(target == NULL ||
            target->IsDied())
        {
            Resume(CONTINUE_TIME);
        }
    }

    void PetAI::OnReturnPeace()
    {
        CPet *pet = (CPet*) GetOwner();
        if(!pet->IsDied() && !pet->IsHangUp())
        {
            ChangeStateTo(CShape::STATE_PEACE);
        }
    }

    void PetAI::OnEvent(const AIEvent &event)
    {
        switch(event.Type())
        {
        case ET_HURT:
            OnHurt();
            break;

        case ET_KILL:
            OnDead();
            break;

        case ET_USESKILLEND:
            {
                const UseSkillEndEvent &uev = (const UseSkillEndEvent&) event;
                OnSkillEnd(uev.type);
            }
            break;
        }
    }
}
