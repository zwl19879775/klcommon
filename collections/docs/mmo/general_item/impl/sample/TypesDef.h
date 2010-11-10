///
/// @file TypesDef.h
/// @author Kevin Lynx
///
#ifndef ___TYPES_DEF_H_
#define ___TYPES_DEF_H_

#include "../GIBase.h"
#include <string>
#include <utility>

#define _TEST_

#ifdef _TEST_
#include "Test.h"
#else
#include "GUID.h"
#endif

namespace GI
{
    class Object;
};

class PropertyKey : public GI::SerialData
{
public:
    PropertyKey( long k = -1 ) : m_key( k )
    {
    }

    /// Compare in std::map.
    bool operator < ( const PropertyKey &other ) const
    {
        return m_key < other.m_key;
    }

    virtual void Serialize( GI::ByteBuffer &buf ) const
    {
        buf.Push( &m_key, sizeof( m_key ) );
    }

    virtual bool UnSerialize( GI::ByteBuffer &buf ) 
    {
        return buf.Pop( &m_key, sizeof( m_key ) );
    }

    operator long () const 
    {
        return m_key;
    }
private:
    long m_key;
};

class PropertyValue : public GI::SerialData
{
public:
    enum ValType{ TINVALID, TLONG, TSTRING, TDOUBLE, TGUID, TPAIR } ;
    typedef long Index;
    typedef CGUID Identify;
    typedef long Stack;
    /// for extend properties.
    typedef std::pair<long, long> LongPair;
public:
    PropertyValue();

    explicit PropertyValue( long v );

    explicit PropertyValue( double v );

    explicit PropertyValue( const CGUID &v );

    explicit PropertyValue( const std::string &v );

    PropertyValue( long first, long second );

    PropertyValue( const LongPair &p );

    PropertyValue( const PropertyValue &other );

    virtual ~PropertyValue();

    bool Valid() const;

    int Type() const { return (int) m_type; }

    PropertyValue &operator = ( const PropertyValue &other );

    bool operator == ( const PropertyValue &other ) const;

    bool operator != ( const PropertyValue &other ) const;

    virtual void Serialize( GI::ByteBuffer &buf ) const;

    virtual bool UnSerialize( GI::ByteBuffer &buf );

    static Stack ToStackCnt( const PropertyValue &val );

    static Identify ToID( const PropertyValue &val );

    static Index ToIndex( const PropertyValue &val );

    static long ToLong( const PropertyValue &val );
    static double ToDouble( const PropertyValue &val );
    static CGUID ToGUID( const PropertyValue &val );
    static std::string ToString( const PropertyValue &val );
    static LongPair ToPair( const PropertyValue &val );
private:

    void Set( int type, const void *val );

    void Free();
private:
    ValType m_type;
    /// Value storage.
    union 
    {
        long lv;
        double *dv;
        std::string *sv;
        CGUID *gv;
        LongPair *pv;
    } m_value;
};

//!
struct TypeSet
{
    typedef PropertyKey KeyType;
    typedef PropertyValue ValueType;
    typedef PropertyValue::Stack StackCntType;
    typedef PropertyValue::Index IndexType;
    typedef PropertyValue::Identify IDType;
};

//!
struct KeySet
{
    static const TypeSet::KeyType IDKey;
    static const TypeSet::KeyType IndexKey;
    static const TypeSet::KeyType StackCntKey;
    static const TypeSet::KeyType MaxStackCntKey;
};

#define KEYTYPE(k) TypeSet::KeyType(k)
#define VALUETYPE(v) TypeSet::ValueType(v)
#define VALUETYPE2(v1,v2) TypeSet::ValueType(v1, v2)

#endif

