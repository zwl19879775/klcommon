///
/// @file IComponent.cpp
/// @author Kevin Lynx
/// 
#include "IComponent.h"
#include "Entity.h"

void IComponent::AddSharedProperties (const SharedProArray &lst)
{
    TRAVERSE_SHAREDPRO_ARRAY (lst, m_entity->AddSharedProperty (SA_VALUE, this));
}

void IComponent::RemoveSharedProperties (const SharedProArray &lst)
{
    TRAVERSE_SHAREDPRO_ARRAY (lst, m_entity->RemoveSharedProperty (SA_VALUE));
}

void IComponent::AddSharedProperties (const std::vector<std::string> &lst)
{
    TRAVERSE_CONST_LIST (std::vector<std::string>, lst, 
        m_entity->AddSharedProperty (L_VALUE, this));
}

void IComponent::RemoveSharedProperties (const std::vector<std::string> &lst)
{
    TRAVERSE_CONST_LIST (std::vector<std::string>, lst, 
        m_entity->RemoveSharedProperty (L_VALUE));
}

void IComponent::AddSharedProperty (const std::string &name)
{
    m_entity->AddSharedProperty (name, this);
}

void IComponent::RemoveSharedProperty (const std::string &name)
{
    m_entity->RemoveSharedProperty (name);
}

