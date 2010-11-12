///
/// @file ObjAmountSender.cpp
///
///
#include "ObjAmountSender.h"
#include "../ByteBufferImpl.h"

ObjAmountSender::ObjAmountSender()
{
    memset( &m_info, 0, sizeof( m_info ) );
}

void ObjAmountSender::Send( const CGUID &shapeID )
{
    CMessage msg( MSG_S2C_CONTAINER_OBJECT_AMOUNT_CHANGE );
    msg.Add( m_info.con.ownerType );
    msg.Add( m_info.con.ownerID );
    msg.Add( m_info.con.conID );
    msg.Add( m_info.con.conPos );
    msg.Add( m_info.obj.type );
    msg.Add( m_info.obj.id );
    msg.Add( m_info.obj.cnt );
    msg.SendToPlayer( shapeID );
}

void ObjAmountSender::SetCon( long type, const CGUID &id, long conID, long pos )
{
    m_info.con.ownerType = type;
    m_info.con.ownerID = id;
    m_info.con.conID = conID;
    m_info.con.conPos = pos;
}

void ObjAmountSender::SetObj( const TypeSet::IDType &id, long cnt )
{
    m_info.obj.type = TYPE_GOODS;
    m_info.obj.id = id;
    m_info.obj.cnt = cnt;
}

