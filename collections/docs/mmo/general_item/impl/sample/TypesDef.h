///
/// @file TypesDef.h
/// @author Kevin Lynx
///
#ifndef ___TYPES_DEF_H_
#define ___TYPES_DEF_H_

#include "../GIBase.h"
#include <string>

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
    PropertyKey( int k = -1 ) : m_key( k )
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
private:
    int m_key;
};

class PropertyValue : public GI::SerialData
{
public:
    typedef long Index;
    typedef CGUID Identify;
    typedef long Stack;
public:
    PropertyValue();

    explicit PropertyValue( long v );

    explicit PropertyValue( double v );

    explicit PropertyValue( const CGUID &v );

    explicit PropertyValue( const std::string &v );

    PropertyValue( const PropertyValue &other );

    virtual ~PropertyValue();

    bool Valid() const;

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
private:

    void Set( int type, const void *val );

    void Free();
private:
    enum Type{ TINVALID, TLONG, TSTRING, TDOUBLE, TGUID } m_type;
    /// Value storage.
    union 
    {
        long lv;
        double *dv;
        std::string *sv;
        CGUID *gv;
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

#endif

