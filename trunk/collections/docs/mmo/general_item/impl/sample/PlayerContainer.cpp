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
    m_owner = NULL;
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

bool PlayerContainer::Move( GI::BaseContainer *srcCon )
{
    return Traverse( MoveOperator( srcCon ) );
}

long PlayerContainer::GetType( GI::BaseContainer *con ) const
{
    if( con == &m_mainCon ) return ConDef::PEI_PACKET;
    for( long type = ConDef::PEI_PACK1; type <= ConDef::PEI_PACK4; ++ type )
    {
        if( con == m_subCons.GetSubCon( ToCellPos( type ) ) ) return type;
    }
    return ConDef::PEI_NONE;
}

long PlayerContainer::ToCellPos( long t ) const
{
    return t - ConDef::PEI_PACK1;
}

