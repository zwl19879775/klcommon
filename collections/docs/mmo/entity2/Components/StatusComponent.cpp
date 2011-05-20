///
/// @file StatusComponent.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "StatusComponent.h"

DECLARE_SHAREDPRO_ARRAY (names) = {
    "DeathState",
    "Control",
    "EquipChange",
    "MoveControl",
    "SkillControl",
    "Move",
    "SpellCast",
    "CombatState",
    "Selection",
    0
};

void StatusComponent::OnAdd ()
{
    Reset ();
    m_refmem.Add (names[0], &m_died);
    m_refmem.Add (names[1], &m_move);
    m_refmem.Add (names[2], &m_inFight);
    m_refmem.Add (names[3], &m_control);
    m_refmem.Add (names[4], &m_chgEquip);
    m_refmem.Add (names[5], &m_canMove);
    m_refmem.Add (names[6], &m_useSkill);
    m_refmem.Add (names[7], &m_skillID);
    m_refmem.Add (names[8], &m_canSel);
    AddSharedProperties (names);
}

void StatusComponent::OnRemove ()
{
    RemoveSharedProperties (names);
}

void StatusComponent::Reset ()
{
    m_died = false;
    m_move = false;
    m_inFight = false;
    m_control = 0;
    m_chgEquip = 0;
    m_canMove = 0;
    m_useSkill = 0;
    m_skillID = 0;
    m_canSel = 0;
}

