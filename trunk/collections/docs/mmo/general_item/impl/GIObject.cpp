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

        bool operator() ( Object::KeyType key, Object::ValueType value )
        {
            int t = PropertyTypeSet::getSingleton().GetType( key );
            if( t & m_type )
            {
                key.Serialize( m_buf );
                value.Serialize( m_buf );
            }
            return false;
        }
        ByteBuffer &m_buf;
        int m_type;
    };

    struct Cloner
    {
        Cloner( Object *dest ) : m_dest( dest ) { }
        bool operator() ( Object::KeyType key, Object::ValueType value ) 
        {
            m_dest->AddProperty( key, value );
            return false;
        }
        Object *m_dest;
    };

    Object::Object( PListenerType *listener ) : SelfType( listener ),
        m_proto( NULL )
    {
        m_dynamicCnt = 0;
        m_generalCnt = 0;
    }

    Object::~Object()
    {
        Clear();
    }

    Object *Object::Clone( Object *dest ) const
    {
        dest->m_proListener = m_proListener;
        dest->m_proto = m_proto;
        dest->Clear();
        Traverse( Cloner( dest ) );
        return dest;
    }

    bool Object::AddProperty( KeyType key, ValueType val )
    {
        bool ret = SelfType::AddProperty( key, val );
        if( ret )
        {
            int t = PropertyTypeSet::getSingleton().GetType( key );
            if( IS_DYNAMIC( t ) ) ++ m_dynamicCnt;
            if( IS_GENERAL( t ) ) ++ m_generalCnt;
        }
        return ret;
    }

    void Object::RemoveProperty( KeyType key )
    {
        int t = PropertyTypeSet::getSingleton().GetType( key );
        if( IS_DYNAMIC( t ) ) -- m_dynamicCnt;
        if( IS_GENERAL( t ) ) -- m_generalCnt;
        SelfType::RemoveProperty( key );
    }

    void Object::Clear()
    {
        m_dynamicCnt = 0;
        m_generalCnt = 0;
        SelfType::Clear();
    }

    Object::ValueType Object::GetValue( KeyType key ) const
    {
        ValueType val = SelfType::GetValue( key );
        if( !val.Valid() )
        {
            val = m_proto->GetValue( key );
        }
        return val;
    }

    void Object::SetProto( const ObjectProto *proto )
    {
        m_proto = proto;
    }

    void Object::SerializeBasic( ByteBuffer &buf ) const
    {
        buf.Push( 2L );
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

    void Object::SerializeDynamic( ByteBuffer &buf ) const
    {
        buf.Push( m_dynamicCnt );
        Traverse( Serializer( buf, PT_DYNAMIC ) );
    }

    bool Object::UnSerializeDynamic( ByteBuffer &buf )
    {
        return UnSerializeProperties( buf );
    }

    void Object::SerializeGeneral( ByteBuffer &buf ) const
    {
        buf.Push( m_generalCnt );
        Traverse( Serializer( buf, PT_GENERAL ) );
    }

    bool Object::UnSerializeGeneral( ByteBuffer &buf )
    {
        return UnSerializeProperties( buf );
    }

    void Object::Serialize( ByteBuffer &buf ) const
    {
        buf.Push( 1L );
        KeySet::IndexKey.Serialize( buf );
        ValueType index = GetValue( KeySet::IndexKey );
        index.Serialize( buf );
        SerializeDynamic( buf );
    }

    bool Object::UnSerialize( ByteBuffer &buf )
    {
        return UnSerializeProperties( buf ) && UnSerializeDynamic( buf );
    }

    bool Object::UnSerializeProperties( ByteBuffer &buf )
    {
        long cnt;
        buf.Pop( &cnt, sizeof( cnt ) );
        for( long i = 0; i < cnt; ++ i )
        {
            KeyType key;
            ValueType value;
            if( !key.UnSerialize( buf ) || !value.UnSerialize( buf ) )
            {
                return false;
            }
            int type = PropertyTypeSet::getSingleton().GetType( key );
            if( IS_DYNAMIC( type ) || IS_INDEX( type ) )
            {
                AddProperty( key, value );
            }
        }
        return true;
    }
}


