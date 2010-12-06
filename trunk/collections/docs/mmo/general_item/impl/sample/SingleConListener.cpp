///
/// @file SingleConListener.cpp
///
///
#include "SingleConListener.h"
#include "ObjVisitor.h"
#include "adapter/ObjOperSender.h"
#include "adapter/ObjAmountSender.h"

SingleConListener::SingleConListener()
{
    m_owner = NULL;
    m_res = NULL;
}

SingleConListener::~SingleConListener()
{
}

void SingleConListener::Begin( CPlayer *owner, ObjOperSender *res, long type )
{
    m_owner = owner;
    m_res = res;
    m_type = type;
}

void SingleConListener::End()
{
    m_owner = NULL;
    m_res = NULL;
    m_type = ConDef::PEI_NONE;
}

void SingleConListener::OnAdd( GI::BaseContainer *con, const GI::Object *obj )
{
    m_res->SetOperType( ConDef::OT_NEW );
    m_res->AddObject( obj );
    m_res->SetDestCon( m_owner->GetExID(), TYPE_PLAYER, m_type, 0 );
    m_res->SetDestObj( ObjVisitor::ID( obj ), ObjVisitor::Count( obj ) );
    m_res->Send( m_owner->GetExID() );
}

void SingleConListener::OnRemove( GI::BaseContainer *con, const GI::Object *obj )
{
    m_res->SetOperType( ConDef::OT_DEL );
    m_res->SetSrcCon( m_owner->GetExID(), TYPE_PLAYER, m_type, 0 );
    m_res->SetSrcObj( ObjVisitor::ID( obj ), ObjVisitor::Count( obj ) );
    m_res->Send( m_owner->GetExID() );
}

void SingleConListener::OnSetAmount( GI::BaseContainer *con, const GI::Object *obj )
{
    ObjAmountSender res;
    res.SetCon( TYPE_PLAYER, m_owner->GetExID(), m_type, 0 );
    res.SetObj( ObjVisitor::ID( obj ), ObjVisitor::Count( obj ) );
    res.Send( m_owner->GetExID() );
}

