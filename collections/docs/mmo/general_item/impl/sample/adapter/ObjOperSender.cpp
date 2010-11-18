///
/// @file ObjOperSender.cpp
///
///
#include "ObjOperSender.h"
#include "Serializer.h"
#include "../ByteBufferImpl.h"
#include "../PlayerContainer.h"

ObjOperSender::ObjOperSender()
{
    m_op = ConDef::OT_INVALID;
    memset( &m_info, 0, sizeof( m_info ) );
}

void ObjOperSender::AddObject( const GI::Object *obj )
{
    m_obj = obj;
}

void ObjOperSender::SetOperType( int op )
{
    m_op = op;
}

void ObjOperSender::SetSrcCon( const CGUID &ownerID, long type, long id, long pos )
{
    m_info.src.ownerID = ownerID;
    m_info.src.ownerType = type;
    m_info.src.conID = id;
    m_info.src.conPos = pos;
}

void ObjOperSender::SetDestCon( const CGUID &ownerID, long type, long id, long pos )
{
    m_info.dest.ownerID = ownerID;
    m_info.dest.ownerType = type;
    m_info.dest.conID = id;
    m_info.dest.conPos = pos;
}

void ObjOperSender::SetSrcObj( const TypeSet::IDType &id, long cnt )
{
    m_info.srcObj.id = id;
    m_info.srcObj.type = TYPE_GOODS;
    m_info.srcObj.cnt = cnt;
}

void ObjOperSender::SetDestObj( const TypeSet::IDType &id, long cnt )
{
    m_info.destObj.id = id;
    m_info.destObj.type = TYPE_GOODS;
    m_info.destObj.cnt = cnt;
}

struct AddCellInfo
{
    AddCellInfo( GI::ByteBuffer &buf ) : m_buf( buf ) { }

    void operator() ( size_t pos, const BaseCellContainer::Cell &cell )
    {
        if( cell.status == BaseCellContainer::Cell::USED )
        {
            m_buf.PushWithSize( &cell.id, sizeof( cell.id ) );
            m_buf.Push( (unsigned short) pos );
        }
    }

    GI::ByteBuffer &m_buf;
};

void ObjOperSender::Send( const CGUID &shapeID )
{
    CMessage msg( MSG_S2C_CONTAINER_OBJECT_MOVE );
    msg.Add( (unsigned char) m_op );
    if( m_op != ConDef::OT_INVALID )
    {
        msg.SendToPlayer( shapeID );
        Reset();
        return;
    }
    msg.Add( m_info.src.ownerType );
    msg.Add( m_info.src.ownerID );
    msg.Add( m_info.src.conID );
    msg.Add( m_info.src.conPos );
    msg.Add( m_info.dest.ownerType );
    msg.Add( m_info.dest.ownerID );
    msg.Add( m_info.dest.conID );
    msg.Add( m_info.dest.conPos );
    msg.Add( m_info.srcObj.type );
    msg.Add( m_info.srcObj.id );
    msg.Add( m_info.destObj.type );
    msg.Add( m_info.destObj.id );
    if( m_op == ConDef::OT_NEW )
    {
        msg.Add( m_info.destObj.cnt );
        if( !m_obj ) return;
        StoreWriteBuffer buf;
        GIAdapter::SerializeObj( m_obj, buf );
        msg.Add( (unsigned long) buf.Size() );
        msg.AddEx( (void*) buf.Data(), buf.Size() );
    }
    else
    {
        msg.Add( m_info.srcObj.cnt );
    }
    if( m_op == ConDef::OT_SWITCH_FILL )
    {
        DBWriteSet db;
        WriteBuffer wbuf( db );
        msg.GetDBWriteSet( db );
        if( !AddReFillInfo( wbuf ) ) return;
    }
    msg.SendToPlayer( shapeID );
    Reset();
}

bool ObjOperSender::AddReFillInfo( GI::ByteBuffer &buf )
{
    if( m_info.dest.conID == ConDef::PEI_PACK )
    {
        CPlayer *player = GetGame()->FindPlayer( m_info.dest.ownerID );
        if( !player ) return false;
        PlayerContainer &allCon = player->GetContainer();
        BaseCellContainer *con = allCon.GetContainer( m_info.dest.conPos );
        if( !con ) return false;

        buf.Push( con->ObjCount() );
        con->TraverseCell( AddCellInfo( buf ) );

        return true;
    }

    return false;
}

