///
/// @file SameConOperator.cpp
///
///
#include "SameConOperator.h"
#include "../CellContainer.h"
#include "../ObjVisitor.h"

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
        // TODO: failure response message.
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
    const CellContainer::Cell &cell = con->GetCell( destPos );
    switch( op )
    {
    case MOVE:
        con->Move( m_info->obj.id, destPos );    
        break;
    case SPLIT:
        con->Split( m_info->obj.id, m_info->obj.cnt, destPos );
        break;
    case PARTIAL_MERGE:
    case MERGE:
        con->Merge( m_info->obj.id, m_info->obj.cnt, destPos );
        break;
    case SWAP:
        con->Swap( m_info->obj.id, cell.id );
        break;
    default:
        return false;
    }
    // TODO: response message.
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
    // TODO: response message.
    return true;
}

