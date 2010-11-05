///
/// @file SubConOperator.cpp
///
///
#include "SubConOperator.h"
#include "../SubContainer.h"
#include "../CellContainer.h"
#include "../ObjVisitor.h"

SubConOperator s_subConOperator;

SubConOperator::SubConOperator()
{
}

SubConOperator::~SubConOperator()
{
}

bool SubConOperator::Check()
{
    return true;
}

bool SubConOperator::Move()
{
    int op = GetOperType();
    if( op == NONE )
    {
        // TODO: failure response.
        return false;
    }
    bool ret = true;
    if( IsSameCon() )
    {
        ret = MoveInSameCon( op );
    }
    else
    {
        ret = MoveInDiffCon( op ); 
    }
    return ret;
}

void SubConOperator::Register()
{
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACKET, TYPE_PLAYER, PEI_PACK, &s_subConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK, TYPE_PLAYER, PEI_PACKET, &s_subConOperator );

    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK1, TYPE_PLAYER, PEI_PACK, &s_subConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK, TYPE_PLAYER, PEI_PACK1, &s_subConOperator );

    REGISTER_HANDLER( TYPE_PLAYER, PEI_DEPOT, TYPE_PLAYER, PEI_PACK, &s_subConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK, TYPE_PLAYER, PEI_DEPOT, &s_subConOperator );

    REGISTER_HANDLER( TYPE_PLAYER, PEI_DEPOT, TYPE_PLAYER, PEI_DEPOTPACK, &s_subConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_DEPOTPACK, TYPE_PLAYER, PEI_DEPOT, &s_subConOperator );
    // ...
}

int SubConOperator::GetOperType()
{
    if( m_info->src.ownerType != TYPE_PLAYER ||
        m_info->dest.ownerType != TYPE_PLAYER ) return NONE;
    if( !IsSameOwner() ) return NONE;
    BaseCellContainer *srcCon = (BaseCellContainer*) GetSrcCon();
    BaseCellContainer *destCon = (BaseCellContainer*) GetDestCon();
    long destPos = m_info->dest.conPos;
    // check first.
    if( m_info->src.conID == PEI_PACK ||
        m_info->src.conID == PEI_DEPOTPACK )
    {
        if( srcCon->ObjCount() > 0 ) return NONE;
    }
    if( m_info->dest.conID == PEI_PACK ||
        m_info->dest.conID == PEI_DEPOTPACK )
    {
        SubContainer *destSubCon = (SubContainer*) destCon;
        // be sure the object is not in the sub container.
        const BaseCellContainer::Cell &destCell = destSubCon->GetCell( destPos );
        if( destCell.u == srcCon ) return NONE;

        const GI::Object *srcObj = srcCon->GetObject( m_info->obj.id );
        if( !srcObj || !ObjVisitor::IsConObject( srcObj ) ) return NONE;
        long srcConSize = ObjVisitor::ConSize( srcObj );
        if( srcConSize < destSubCon->SubObjCount( destPos ) ) return NONE;
    }
    const BaseCellContainer::Cell &destCell = destCon->GetCell( destPos );
    if( destCell.status == BaseCellContainer::Cell::DISABLED ) return NONE;
    if( destCell.status == BaseCellContainer::Cell::EMPTY ) return MOVE;
    return SWAP;
}

bool SubConOperator::MoveInDiffCon( int op )
{
    long destPos = m_info->dest.conPos;
    BaseCellContainer *srcCon = (BaseCellContainer*) GetSrcCon();
    if( m_info->dest.conID == PEI_PACK ||
        m_info->dest.conID == PEI_DEPOTPACK )
    {
        SubContainer *destCon = (SubContainer*) GetDestCon();
        if( op == MOVE )
        {
            destCon->Move( srcCon, m_info->obj.id, destPos );
        }
        else if( op == SWAP )
        {
            const BaseCellContainer::Cell &cell = destCon->GetCell( destPos );
            if( cell.status != BaseCellContainer::Cell::USED ) return false;
            destCon->Swap( srcCon, m_info->obj.id, cell.id );
        }
    }
    else if( m_info->src.conID == PEI_PACK ||
             m_info->src.conID == PEI_DEPOTPACK )
    {
        CellContainer *destCon = (CellContainer*) GetDestCon();
        if( op == MOVE )
        {
            long cnt = ObjVisitor::Count( srcCon->GetObject( m_info->obj.id ) );
            destCon->Move( srcCon, m_info->obj.id, cnt, destPos );
        }
        else if( op == SWAP )
        {
            const BaseCellContainer::Cell &destCell = destCon->GetCell( destPos );
            if( destCell.status != BaseCellContainer::Cell::USED ) return false;
            destCon->Swap( srcCon, m_info->obj.id, destCell.id );
        }
    }
    return true;
}

bool SubConOperator::MoveInSameCon( int op )
{
    SubContainer *con = (SubContainer*) GetSrcCon();
    if( op == MOVE )
    {
        con->Move( m_info->obj.id, m_info->dest.conPos );
    }
    else if( op == SWAP )
    {
        const BaseCellContainer::Cell &cell = con->GetCell( m_info->dest.conPos );
        if( cell.status != BaseCellContainer::Cell::USED ) return false;
        con->Swap( m_info->obj.id, cell.id );
    }
    return true;
}

