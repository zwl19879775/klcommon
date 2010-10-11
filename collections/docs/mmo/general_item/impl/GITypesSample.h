///
/// @file GITypesSample.h
/// @author Kevin Lynx
/// @brief Sample types definition file.
///
#ifndef __GI_TYPES_SAMPLE_H_
#define __GI_TYPES_SAMPLE_H_

#include "GIBase.h"
#include <string>

namespace Sample
{
    class Key : public GI::SerialData
    {
    public:
        Key( long k ) : key( k )
        {
        }
    
        Key() : key( 0 )
        {
        }

        //!
        bool operator< ( const Key &other ) const
        {
            return key < other.key;
        }

        void Serialize( GI::ByteBuffer &buf ) const
        {
            buf.Push( &key, sizeof( key ) );
        }

        bool UnSerialize( GI::ByteBuffer &buf )
        {
            return buf.Pop( &key, sizeof( key ) );
        }

    private:
        long key;
    };

    class Value : public GI::SerialData
    {
    public:
        typedef long Index;
        typedef long Identify;
        typedef long Stack;
        // ...
    public:
        //!
        Value() : type( TINVALID )
        {
        }

        //!
        explicit Value( long val ) : type( TLONG)
        {
            value.lv = new long( val );
        }

        virtual ~Value()
        {
            // switch type to delete.
        }

        //!
        bool Valid() const
        {
            return type != TINVALID;
        }

        Value &operator = ( const Value &other )
        {
            // copy.
            return *this;
        }

        bool operator == ( const Value &other ) const;

        bool operator != ( const Value &other ) const
        {
            return !( *this == other );
        }

        //!
        static Stack ToStackCnt( const Value &val )
        {
            return *val.value.lv;
        }

        //!
        static Identify ToID( const Value &val )
        {
            return *val.value.lv;
        }

        void Serialize( GI::ByteBuffer &buf ) const
        {
        }

        bool UnSerialize( GI::ByteBuffer &buf )
        {
            return true;
        }

    private:
        union { long *lv; std::string *sv; double *dv; } value;
        enum { TINVALID, TLONG, TSTRING, TDOUBLE } type;
    };
}

//! 
struct TypeSet
{
    typedef Sample::Key KeyType;
    typedef Sample::Value ValueType;
    typedef Sample::Value::Stack StackCntType;
    typedef Sample::Value::Index IndexType;
    typedef Sample::Value::Identify IDType;
};

//!
struct KeySet
{
    static const TypeSet::KeyType IDKey;
    static const TypeSet::KeyType IndexKey;
    static const TypeSet::KeyType StackCntKey;
    static const TypeSet::KeyType MaxStackCntKey;
};

#endif

