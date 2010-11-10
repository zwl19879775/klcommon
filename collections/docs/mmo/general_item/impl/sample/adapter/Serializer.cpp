///
/// @file Serializer.cpp
///
///
#include "Serializer.h"
#include "../ObjVisitor.h"
#include "../GoodsPropertyType.h"
#include "../SubContainer.h"
#include "../CellContainer.h"
#include <map>

#define SERIALIZE_P( obj, buf, k ) \
    { \
        TypeSet::ValueType v = obj->GetValue( KEYTYPE( k ) ); \
        SerializeValue( v, buf ); \
    }

namespace GIAdapter
{
    struct SerializeExt
    {
        void operator() ( const TypeSet::KeyType &key, const TypeSet::ValueType &val )
        {
            if( (long) key > PEXTEND_BEGIN && (long) key < PEXTEND_END )
            {
                m_ps[&key] = &val;
            }
        }

        void Serialize( GI::ByteBuffer &buf )
        {
            buf.Push( m_ps.size() );
            for( Properties::iterator it = m_ps.begin(); it != m_ps.end(); ++ it )
            {
                const TypeSet::KeyType *key = it->first;
                buf.Push( (short) (long)(*key) );
                SerializeValue( *it->second, buf );
            }
        }

        typedef std::map<const TypeSet::KeyType*, const TypeSet::ValueType*> Properties;
        Properties m_ps;
    };

    bool SerializeObj( const GI::Object *obj, GI::ByteBuffer &buf )
    {
        SERIALIZE_P( obj, buf, PINDEX ); 
        SERIALIZE_P( obj, buf, PID ); 
        SERIALIZE_P( obj, buf, PSTACKCNT ); 
        SERIALIZE_P( obj, buf, PMAKER_NAME ); 
        SERIALIZE_P( obj, buf, PGOLD_PRICE ); 
        SERIALIZE_P( obj, buf, PSILVER_PRICE ); 
        SERIALIZE_P( obj, buf, PBUY_PRICE ); 
        SERIALIZE_P( obj, buf, PTYPE ); 

        SerializeExt serializer;
        obj->Traverse( serializer );
        serializer.Serialize( buf );

        // Rgn position is not useful here.
        buf.Push( 0L );
        buf.Push( 0L );

        // Card.
        buf.Push( 0L );

        return true;
    }

    bool SerializeGeneral( const GI::Object *obj, GI::ByteBuffer &buf )
    {
        SERIALIZE_P( obj, buf, PINDEX );
        SERIALIZE_P( obj, buf, PSTACKCNT );
        SERIALIZE_P( obj, buf, PRGNX );
        SERIALIZE_P( obj, buf, PRGNY );
        // Weapon level.
        buf.Push( 0L );
        return true;
    }

    bool SerializeValue( const TypeSet::ValueType &val, GI::ByteBuffer &buf )
    {
        switch( val.Type() )
        {
        case TypeSet::ValueType::TLONG:
            {
                buf.Push( TypeSet::ValueType::ToLong( val ) );
            }
            break;
        case TypeSet::ValueType::TSTRING:
            {
                std::string s = TypeSet::ValueType::ToString( val );
                buf.Push( s.size() );
                buf.Push( s.c_str(), s.size() );
            }
            break;
        case TypeSet::ValueType::TDOUBLE:
            {
                buf.Push( TypeSet::ValueType::ToDouble( val ) );
            }
            break;
        case TypeSet::ValueType::TGUID:
            {
                char s = sizeof( CGUID );
                CGUID guid = TypeSet::ValueType::ToGUID( val );
                buf.Push( s );
                buf.Push( &guid, s );
            }
            break;
        case TypeSet::ValueType::TPAIR:
            {
                TypeSet::ValueType::LongPair p = TypeSet::ValueType::ToPair( val );
                buf.Push( p.first );
                buf.Push( p.second );
            }
            break;
        default:
            return false;
        }
        return true;
    }

    struct SerializeCell
    {
        SerializeCell( GI::ByteBuffer &buf ) : m_buf( buf ) { }

        void operator() ( const TypeSet::IDType &id, const GI::Object *obj )
        {
            // Unknown flag.
            m_buf.Push( false );
            m_buf.Push( (short) ObjVisitor::Count( obj ) );
            m_buf.Push( (unsigned char) ObjVisitor::Pos( obj ) );
            SerializeObj( obj, m_buf );
        }

        GI::ByteBuffer &m_buf;
    };

    bool SerializeContainer( const BaseCellContainer *con, GI::ByteBuffer &buf )
    {
        buf.Push( con->ObjCount() );
        con->Traverse( SerializeCell( buf ) );
        return true;
    }

    bool SerializeSubContainer( const SubContainer *con, GI::ByteBuffer &buf )
    {
        buf.Push( con->UsedSize() );
        for( long i = 0; i < con->Size(); ++ i )
        {
            const SubContainer::Cell &cell = con->GetCell( i );
            if( cell.status == SubContainer::Cell::USED )
            {
                const GI::Object *obj = con->GetObject( cell.id );
                SerializeObj( obj, buf );
                buf.Push( (unsigned char) i );
            }
        }

        for( long i = 0; i < con->Size(); ++ i )
        {
            const SubContainer::Cell &cell = con->GetCell( i );
            if( cell.status != SubContainer::Cell::USED )
            {
                buf.Push( 0L );
            }
            else
            {
                SerializeContainer( CAST_CELL( cell.u ), buf );
            }
        }
        return true;
    }
}

