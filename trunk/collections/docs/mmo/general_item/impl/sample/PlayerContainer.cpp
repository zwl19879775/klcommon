///
/// @file PlayerContainer.cpp
///
///
#include "PlayerContainer.h"
#include "adapter/Serializer.h"
#include "adapter/ObjOperSender.h"
#include "PlayerConListener.h"

struct MoveOperator
{
    MoveOperator( GI::BaseContainer *con ) : m_con( con ) { }

    bool operator() ( long type, CellContainer *con )
    {
        return con->MoveAll( m_con );
    }

    GI::BaseContainer *m_con;
};

PlayerContainer::PlayerContainer()
{
    m_mainCon.ReSize( ConDef::MAIN_CON_SIZE );
    m_mainCon.EnableAll();
    m_subCons.ReSize( ConDef::SUB_CON_SIZE );
    m_subCons.EnableAll();
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
    PlayerConListener listener;
    ObjOperSender res;
    listener.Begin( this, &res );
    bool ret = Traverse( MoveOperator( srcCon ) );
    listener.End();
    return ret;
}

void PlayerContainer::SerializeToClient( GI::ByteBuffer &buf ) const
{
    buf.Push( 4L );
    GIAdapter::SerializeContainer( &m_mainCon, buf );
    GIAdapter::SerializeSubContainer( &m_subCons, buf );
    // TODO: wallet
    buf.Push( 5L );
    buf.Push( 0L );
    buf.Push( NULL_GUID );
    buf.Push( 0L );
    buf.Push( NULL_GUID );
    buf.Push( 0L );
    buf.Push( NULL_GUID );
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

