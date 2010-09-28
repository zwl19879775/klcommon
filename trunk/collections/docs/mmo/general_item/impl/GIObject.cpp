///
/// @file GIObject.cpp
/// @author Kevin Lynx
///
#include "GIObject.h"
#include "GIObjectProto.h"

namespace GI
{
    //
    // Used to traverse properties to serialize.
    //
    struct Serializer
    {
        Serializer( ByteBuffer &buf, int type ) :
           m_buf( buf ), m_type( type ) { }

        void operator() ( Object::KeyType key, Object::ValueType value )
        {
            int t = PropertyTypeSet::getSingleton().GetType( key );
            if( t & m_type )
            {
                key.Serialize( m_buf );
                value.Serialize( m_buf );
            }
        }
        ByteBuffer &m_buf;
        int m_type;
    };

    Object::Object( PListenerType *listener ) : SelfType( listener ),
        m_proto( NULL )
    {
        m_detailCnt = 0;
        m_generalCnt = 0;
    }

    Object::~Object()
    {
        Clear();
    }

    bool Object::AddProperty( KeyType key, ValueType val )
    {
        bool ret = SelfType::AddProperty( key, val );
        if( ret )
        {
            int t = PropertyTypeSet::getSingleton().GetType( key );
            if( IS_DETAIL( t ) ) ++ m_detailCnt;
            if( IS_GENERAL( t ) ) ++ m_generalCnt;
        }
        return ret;
    }

    void Object::RemoveProperty( KeyType key )
    {
        int t = PropertyTypeSet::getSingleton().GetType( key );
        if( IS_DETAIL( t ) ) -- m_detailCnt;
        if( IS_GENERAL( t ) ) -- m_generalCnt;
        SelfType::RemoveProperty( key );
    }

    void Object::Clear()
    {
        m_detailCnt = 0;
        m_generalCnt = 0;
        SelfType::Clear();
    }

    Object::ValueType Object::GetValue( KeyType key ) const
    {
        ValueType val = SelfType::GetValue( key );
        if( val == ValueType() )
        {
            val = m_proto->GetValue( key );
        }
        return val;
    }

    void Object::SetProto( const ObjectProto *proto )
    {
        m_proto = proto;
    }

    void Object::SerializeBasic( ByteBuffer &buf )
    {
        int cnt = 2;
        buf.Push( &cnt, sizeof( cnt ) );
        KeySet::IndexKey.Serialize( buf );
        ValueType index = GetValue( KeySet::IndexKey );
        index.Serialize( buf );
        KeySet::IDKey.Serialize( buf );
        ValueType id = GetValue( KeySet::IDKey );
        id.Serialize( buf );
    }

    bool Object::UnSerializeBasic( ByteBuffer &buf )
    {
        return UnSerializeProperties( buf );
    }

    void Object::Serialize( ByteBuffer &buf )
    {
        Traverse( Serializer( buf, PT_DYNAMIC ) );
    }

    bool Object::UnSerialize( ByteBuffer &buf )
    {
        return UnSerializeProperties( buf );
    }

    void Object::SerializeDetail( ByteBuffer &buf )
    {
        Traverse( Serializer( buf, PT_DETAIL ) );
    }

    bool Object::UnSerializeDetail( ByteBuffer &buf )
    {
        return UnSerializeProperties( buf );
    }

    void Object::SerializeGeneral( ByteBuffer &buf )
    {
        Traverse( Serializer( buf, PT_GENERAL ) );
    }

    bool Object::UnSerializeGeneral( ByteBuffer &buf )
    {
        return UnSerializeProperties( buf );
    }

    bool Object::UnSerializeProperties( ByteBuffer &buf )
    {
        int cnt;
        buf.Pop( &cnt, sizeof( cnt ) );
        for( int i = 0; i < cnt; ++ i )
        {
            KeyType key;
            ValueType value;
            if( !key.UnSerialize( buf ) || !value.UnSerialize( buf ) )
            {
                return false;
            }
            int type = PropertyTypeSet::getSingleton().GetType( key );
            if( IS_DYNAMIC( type ) )
            {
                AddProperty( key, value );
            }
        }
        return true;
    }
}


