///
/// @file GIObjectProto.cpp
/// @author Kevin Lynx
///
#include "GIObjectProto.h"

namespace GI
{
    struct Serializer
    {
        Serializer( ByteBuffer &buf ) : m_buf( buf ) { }

        bool operator() ( const ObjectProto::KeyType &k,  
                const ObjectProto::ValueType &v ) const
        {
            k.Serialize( m_buf );
            v.Serialize( m_buf );
            return false;
        }

        bool operator() ( const ObjProtoFactory::KeyType &k,
                const ObjProtoFactory::ValueType &v ) const
        {
            v->Serialize( m_buf );
            return false;
        }

        ByteBuffer &m_buf;
    };

    void ObjectProto::Serialize( ByteBuffer &buf ) const
    {
        buf.Push( Size() );
        Traverse( Serializer( buf ) );
    }

    bool ObjectProto::UnSerialize( ByteBuffer &buf )
    {
        Clear();
        size_t size;
        if( !buf.Pop( &size ) ) return false;
        for( size_t i = 0; i < size; ++ i )
        {
            KeyType key;
            ValueType value;
            key.UnSerialize( buf );
            value.UnSerialize( buf );
            AddProperty( key, value );
        } 
        return true;
    }

    ObjProtoFactory::ObjProtoFactory( ProtoLoader *loader ) :
        SelfType( NULL ), m_loader( loader )
    {
    }

    ObjProtoFactory::~ObjProtoFactory()
    {
        Release();
    }

    bool ObjProtoFactory::Load( void *u )
    {
        return m_loader->Load( this, u );
    }

    void ObjProtoFactory::Release()
    {
        for( TableType::iterator it = m_properties.begin();
                it != m_properties.end(); ++ it )
        {
            delete it->second;
        }
        Clear();
    }

    void ObjProtoFactory::Serialize( ByteBuffer &buf ) const
    {
        buf.Push( Size() );
        Traverse( Serializer( buf ) );
    }

    bool ObjProtoFactory::UnSerialize( ByteBuffer &buf )
    {
        Release();
        size_t size;
        if( !buf.Pop( &size ) ) return false;
        for( size_t i = 0; i < size; ++ i )
        {
            ObjectProto *proto = new ObjectProto();
            if( !proto->UnSerialize( buf ) ) delete proto;
            else
            {
                TypeSet::IndexType index = TypeSet::ValueType::ToIndex( proto->GetValue(
                            KeySet::IndexKey ) );
                AddProperty( index, proto );
            }
        }
        return true;
    }

    const ObjectProto *ObjProtoFactory::GetProto( TypeSet::IndexType index ) const
    {
        return GetValue( index );
    }
}

