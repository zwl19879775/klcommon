///
/// @file PlayerConListener.cpp
///
///
#include "PlayerConListener.h"
#include "PlayerContainer.h"
#include "ObjVisitor.h"
#include "adapter/ObjOperSender.h"
#include "adapter/ObjAmountSender.h"

struct SetAllListener
{
    SetAllListener( PlayerConListener *lis ) : m_listener( lis ) { }

    bool operator() ( long type, BaseCellContainer *con )
    {
        con->SetListener( m_listener );
        return false;
    }

    PlayerConListener *m_listener;
};

PlayerConListener::PlayerConListener()
{
    m_con = NULL;
    m_res = NULL;
}

PlayerConListener::~PlayerConListener()
{
}

void PlayerConListener::Begin( PlayerContainer *con, ObjOperSender *res )
{
    m_res = res;
    m_con = con;
    con->Traverse( SetAllListener( this ) );
}

void PlayerConListener::End()
{
    m_res = NULL;
    m_con->Traverse( SetAllListener( NULL ) );
    m_con = NULL;
}

void PlayerConListener::OnAdd( GI::BaseContainer *con, const GI::Object *obj )
{
    long type = m_con->GetType( con );
    if( type == ConDef::PEI_NONE ) return;
    long pos = ObjVisitor::Pos( obj );
    m_res->SetDestCon( m_con->GetOwner()->GetExID(), TYPE_PLAYER, type, pos );
    m_res->SetDestObj( ObjVisitor::ID( obj ), ObjVisitor::Count( obj ) );
}

void PlayerConListener::OnRemove( GI::BaseContainer *con, const GI::Object *obj )
{
    long type = m_con->GetType( con );
    if( type == ConDef::PEI_NONE ) return;
    long pos = ObjVisitor::Pos( obj );
    m_res->SetSrcCon( m_con->GetOwner()->GetExID(), TYPE_PLAYER, type, pos );
    m_res->SetSrcObj( ObjVisitor::ID( obj ), ObjVisitor::Count( obj ) );
}

void PlayerConListener::OnMoved( GI::BaseContainer *srcCon, GI::BaseContainer *destCon, 
            const GI::Object *obj )
{
    long srcType = m_con->GetType( srcCon );
    long destType = m_con->GetType( destCon );
    if( destType == ConDef::PEI_NONE ) return;
    if( srcType == ConDef::PEI_NONE ) // player got goods
    {
        m_res->SetOperType( ConDef::OT_NEW );
        m_res->AddObject( obj );
        m_res->Send( m_con->GetOwner()->GetExID() );
    }
}

void PlayerConListener::OnMerged( GI::MergeContainer *con, const GI::Object *obj, const GI::Object *mergeObj )
{
    m_res->Reset();
    ObjAmountSender res;
    long type = m_con->GetType( con );
    if( type == ConDef::PEI_NONE ) return;
    long pos = ObjVisitor::Pos( obj );
    res.SetCon( TYPE_PLAYER, m_con->GetOwner()->GetExID(), type, pos );
    res.SetObj( ObjVisitor::ID( obj ), ObjVisitor::Count( obj ) );
    res.Send( m_con->GetOwner()->GetExID() );
}

