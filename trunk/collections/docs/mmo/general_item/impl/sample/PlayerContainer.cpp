///
/// @file PlayerContainer.cpp
///
///
#include "PlayerContainer.h"

struct MoveOperator
{
    MoveOperator( GI::BaseContainer *con ) : m_con( con ) { }

    bool operator() ( long type, BaseCellContainer *con )
    {
        return con->MoveAll( m_con );
    }

    GI::BaseContainer *m_con;
};

PlayerContainer::PlayerContainer()
{
}

PlayerContainer::~PlayerContainer()
{
}

BaseCellContainer *PlayerContainer::GetContainer( long type )
{
    if( type == ConDef::PEI_PACKET )
    {
        return &m_mainCon;
    }
    if( type == ConDef::PEI_PACK )
    {
        return &m_subCons;
    }
    if( type >= ConDef::PEI_PACK1 && 
        type <= ConDef::PEI_PACK4 )
    {
        return m_subCons.GetSubCon( ToCellPos( type ) );
    }
    return NULL;
}

bool PlayerContainer::Move( GI::BaseContainer *srcCon, PlayerContainer *destCon )
{
    return destCon->Traverse( MoveOperator( srcCon ) );
}

long PlayerContainer::ToCellPos( long t )
{
    return t - ConDef::PEI_PACK1;
}

