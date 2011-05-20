///
/// @file VisualComponent.cpp
/// @author Kevin Lynx
///
//#include "stdafx.h"
#include "VisualComponent.h"

DECLARE_SHAREDPRO_ARRAY (names) = {
    "Ride",
    "Body",
    "HairStyle",
    "HairColor",
    "Jewelry",
    "Face",
    "Size",
    "Interface",
    "HpBarVisual",
    0
};

void SocialComponent::OnAdd ()
{
    Reset ();
    m_refmem.Add (names[0], &m_ride);
    m_refmem.Add (names[1], &m_body);
    m_refmem.Add (names[2], &m_hairStyle);
    m_refmem.Add (names[3], &m_hairColor);
    m_refmem.Add (names[4], &m_jewelry);
    m_refmem.Add (names[5], &m_face);
    m_refmem.Add (names[6], &m_size);
    m_refmem.Add (names[7], &m_ui);
    AddSharedProperties (names);
}

void SocialComponent::OnRemove ()
{
    RemoveSharedProperties (names);
}

void SocialComponent::Reset ()
{
    m_ride = 0;
    m_body = 0;
    m_hairStyle = 0;
    m_hairColor = 0;
    m_jewelry = 0;
    m_face = 0;
    m_size = 0;
    m_ui = 0;
}

