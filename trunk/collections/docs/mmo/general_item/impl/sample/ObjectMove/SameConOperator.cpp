///
/// @file SameConOperator.cpp
///
///
#include "SameConOperator.h"
#include "../CellContainer.h"
#include "../ObjVisitor.h"
#include "../ContainerDef.h"
#include "../adapter/ObjOperSender.h"   

SameConOperator s_sameConOperator;

SameConOperator::SameConOperator()
{
}

SameConOperator::~SameConOperator()
{
}

bool SameConOperator::Check()
{
    return true;
}

bool SameConOperator::Move()
{
    bool ret = true;
    if( IsSameOwner() && IsSameCon() )
    {
        ret = MoveInSameCon();
    }
    else if( IsSameOwner() )
    {
        ret = MoveInDiffCon();
    }
    if( !ret )
    {
        m_res->SetOperType( ConDef::OT_INVALID );
    }
    return ret;
}

void SameConOperator::Register()
{
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACKET, TYPE_PLAYER, PEI_PACKET, &s_sameConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACKET, TYPE_PLAYER, PEI_PACK1, &s_sameConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACKET, TYPE_PLAYER, PEI_PACK2, &s_sameConOperator );

    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK1, TYPE_PLAYER, PEI_PACKET, &s_sameConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK2, TYPE_PLAYER, PEI_PACKET, &s_sameConOperator );
}

bool SameConOperator::MoveInSameCon()
{
    int op = GetCellOperType();
    CellContainer *con = (CellContainer*) GetSrcCon();
    long destPos = m_info->dest.conPos;
    CellContainer::Cell cell = con->GetCell( destPos );
    switch( op )
    {
    case MOVE:
        {
            con->Move( m_info->obj.id, destPos );    
            m_res->SetOperType( ConDef::OT_MOVE );
            m_res->SetSrcCon( m_info->src.ownerID, m_info->src.ownerType, m_info->src.conID, m_info->src.conPos );
            m_res->SetDestCon( m_info->dest.ownerID, m_info->src.ownerType, m_info->dest.conID, destPos );
            const GI::Object *obj = con->GetObject( m_info->obj.id );
            long cnt = ObjVisitor::Count( obj );
            m_res->SetSrcObj( m_info->obj.id, cnt );
            m_res->SetDestObj( m_info->obj.id, cnt );
        }
        break;
    case SPLIT:
        con->Split( m_info->obj.id, m_info->obj.cnt, destPos );
        break;
    case PARTIAL_MERGE:
    case MERGE:
        con->Merge( m_info->obj.id, m_info->obj.cnt, destPos );
        break;
    case SWAP:
        {
            con->Swap( m_info->obj.id, cell.id );
            m_res->SetOperType( ConDef::OT_SWITCH_OBJ );
            m_res->SetSrcCon( m_info->src.ownerID, m_info->src.ownerType, m_info->src.conID, m_info->src.conPos );
            m_res->SetDestCon( m_info->dest.ownerID, m_info->dest.ownerType, m_info->dest.conID, m_info->dest.conPos );

            const GI::Object *destObj = con->GetObject( cell.id );
            m_res->SetDestObj( cell.id, ObjVisitor::Count( destObj ) );
            m_res->SetSrcObj( m_info->obj.id, m_info->obj.cnt );
        }
        break;
    default:
        return false;
    }
    return true;
}

bool SameConOperator::MoveInDiffCon()
{
    CellContainer *srcCon = dynamic_cast<CellContainer*>( GetSrcCon() );
    CellContainer *destCon = dynamic_cast<CellContainer*>( GetDestCon() );
    if( !srcCon || !destCon ) return false;
    long destPos = m_info->dest.conPos;
    long operCnt = m_info->obj.cnt;
    const CellContainer::Cell &cell = destCon->GetCell( destPos );
    int op = GetCellOperType();
    switch( op )
    {
    case MOVE:
        destCon->Move( srcCon, m_info->obj.id, operCnt, destPos );
        break;
    case PARTIAL_MERGE:
    case MERGE:
        destCon->Merge( srcCon, m_info->obj.id, operCnt, destPos );
        break;
    case SWAP:
        destCon->Swap( srcCon, m_info->obj.id, cell.id );
        break;
    default:
        return false;
    }
    return true;
}

