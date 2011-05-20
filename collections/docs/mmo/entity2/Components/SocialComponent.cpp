///
/// @file SocialComponent.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "SocialComponent.h"

DECLARE_SHAREDPRO_ARRAY (names) = {
    "ChasGuildGUID",
    "ChasGuidPosition",
    "ChasTeamGUID",
    "ChasTeamPosition",
    "Faction",
    0
};

void SocialComponent::OnAdd ()
{
    Reset ();
    m_refmem.Add (names[0], &m_guildID);
    m_refmem.Add (names[1], &m_guildPos);
    m_refmem.Add (names[2], &m_teamID);
    m_refmem.Add (names[3], &m_teamPos);
    m_refmem.Add (names[4], &m_faction);
    AddSharedProperties (names);
}

void SocialComponent::OnRemove ()
{
    RemoveSharedProperties (names);
}

void SocialComponent::Reset ()
{
    m_guildID = NULL_GUID;
    m_guildPos = 0;
    m_faction = 0;
    m_teamID = NULL_GUID;
    m_teamPos = 0;
}

