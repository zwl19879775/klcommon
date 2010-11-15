///
/// @file ObjPropertyNotifier.cpp
///
///
#include "ObjPropertyNotifier.h"
#include "../ObjVisitor.h"
#include "../GoodsPropertyType.h"
#include "../ByteBufferImpl.h"

void ObjPropertyNotifier::OnSet( TypeSet::KeyType key, TypeSet::ValueType oldVal, 
        TypeSet::ValueType newVal )
{
    // only notify addon properties.
    long lkey = (long) key;
    if( lkey > PEXTEND_BEGIN && lkey < PEXTEND_END )
    {
        Dirty d;
        d.key = key;
        typedef TypeSet::ValueType::LongPair LongPair;
        LongPair oldv = oldVal.ToPair( oldVal );
        LongPair newv = newVal.ToPair( newVal );
        d.pos = oldv.first == newv.first ? 1 : 0;
        m_dirtyList.push_back( d );
    }
}

bool ObjPropertyNotifier::Update( const CGUID &shapeID )
{
    CMessage msg( MSG_S2C_PLAYER_CHANGE_PROPERTIES );
    DBWriteSet db;
    msg.GetDBWriteSet( db );
    WriteBuffer wbuf( db );
    GI::ByteBuffer &buf = wbuf;
    const GI::Object *owner = (const GI::Object*) m_owner;
    TypeSet::IDType id = ObjVisitor::ID( owner );
    buf.PushWithSize( &id, sizeof( id ) );
    buf.Push( m_dirtyList.size() );
    for( DirtyListT::iterator it = m_dirtyList.begin();
           it != m_dirtyList.end(); ++ it )
    {
        Dirty &d = *it;
        d.key.Serialize( buf );
        buf.Push( d.pos );
        TypeSet::ValueType val = owner->GetValue( d.key );
        TypeSet::ValueType::LongPair valp = val.ToPair( val );
        if( d.pos == 0 )
        {
            buf.Push( valp.first );
        }
        else
        {
            buf.Push( valp.second );
        }
    } 
    msg.SendToPlayer( shapeID );
    m_dirtyList.clear();
    return true;
}

