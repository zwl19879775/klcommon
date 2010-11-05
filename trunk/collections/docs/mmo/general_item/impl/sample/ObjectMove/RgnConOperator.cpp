///
/// @file RgnConOperator.cpp
///
///
#include "RgnConOperator.h"
#include "../RgnContainer.h"
#include "../CellContainer.h"
#include "../ObjVisitor.h"

RgnConOperator s_rgnConOperator;

RgnConOperator::RgnConOperator()
{
}

RgnConOperator::~RgnConOperator()
{
}

bool RgnConOperator::Check()
{
    return true;
}

bool RgnConOperator::Move()
{
    bool ret = true;
    if( m_info->dest.ownerType == TYPE_REGION )
    {
        ret = Drop();
    }
    else if( m_info->dest.ownerType == TYPE_PLAYER )
    {
        ret = Pickup();
    }
    return ret;
}

void RgnConOperator::Register()
{
    REGISTER_HANDLER( TYPE_REGION, 0, TYPE_PLAYER, 0, &s_rgnConOperator );

    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACKET, TYPE_REGION, 0, &s_rgnConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK1, TYPE_REGION, 0, &s_rgnConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK2, TYPE_REGION, 0, &s_rgnConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK3, TYPE_REGION, 0, &s_rgnConOperator );
    REGISTER_HANDLER( TYPE_PLAYER, PEI_PACK4, TYPE_REGION, 0, &s_rgnConOperator );
}

bool RgnConOperator::Drop()
{
    CellContainer *srcCon = (CellContainer*) GetSrcCon();
    if( !srcCon ) return false;
    RgnContainer *destCon = (RgnContainer*) GetDestCon();
    if( !destCon ) return false;
    // TODO: generate drop position by player position.
    RgnContainer::GoodsPos pos;
    return destCon->Move( srcCon, m_info->obj.id, pos, m_info->obj.cnt );
}

bool RgnConOperator::Pickup()
{
    /* TODO:
    for_each container of a player [packet/pack1/pack2/pack3/pack4]
        if( playerCon->Move( srcCon, m_info->obj.id ) ) 
            success
            send response message
    */
    return true;
}

