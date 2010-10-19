///
/// @file TypesDef.cpp
/// @author Kevin Lynx
///
#include "TypesDef.h"
#include "GoodsPropertyType.h"
#include <assert.h>

PropertyValue::PropertyValue() : m_type( TINVALID )
{
    m_value.lv = 0;
}

PropertyValue::PropertyValue( long v ) 
{
    Set( TLONG, (void*) v );
}

PropertyValue::PropertyValue( double v ) 
{
    Set( TDOUBLE, &v );
}

PropertyValue::PropertyValue( const CGUID &v ) 
{
    Set( TGUID, &v );
}

PropertyValue::PropertyValue( const std::string &v )
{
    Set( TSTRING, &v );
}

PropertyValue::PropertyValue( const PropertyValue &other )
{
    *this = other;
}

PropertyValue::~PropertyValue()
{
    Free();
}

bool PropertyValue::Valid() const
{
    return m_type != TINVALID;
}

PropertyValue &PropertyValue::operator = ( const PropertyValue &other )
{
    if( &other == this ) return *this;
    Free();
    Set( other.m_type, other.m_value.dv );
    return *this;
}

bool PropertyValue::operator == ( const PropertyValue &other ) const
{
    if( this == &other ) return true;
    if( m_type != other.m_type ) return false;
    bool ret = false;
    switch( m_type )
    {
      case TLONG:
          ret = m_value.lv == other.m_value.lv;
          break;
      case TDOUBLE:
          ret = *m_value.dv == *other.m_value.dv;
          break;
      case TSTRING:
          ret = *m_value.sv == *other.m_value.sv;
          break;
      case TGUID:
          ret = *m_value.gv == *other.m_value.gv;
          break;
      case TINVALID:
          ret = true; // ?
          break;
    }
    return ret;
}

bool PropertyValue::operator != ( const PropertyValue &other ) const
{
    return !(*this == other );
}

void PropertyValue::Serialize( GI::ByteBuffer &buf ) const
{
    buf.Push( &m_type, sizeof( m_type ) );
    switch( m_type )
    {
      case TLONG:
          buf.Push( &m_value.lv, sizeof( m_value.lv ) );
          break;
      case TDOUBLE:
          buf.Push( m_value.dv, sizeof( *m_value.dv ) );
          break;
      case TSTRING: // special
          {
              size_t len = m_value.sv->length();
              buf.Push( &len, sizeof( len ) );
              buf.Push( m_value.sv->c_str(), m_value.sv->length() );
          }
          break;
      case TGUID: // TODO: how to serialize GUID depents on GUID impl.
          buf.Push( m_value.gv, sizeof( *m_value.gv ) );
      default:
        break;
    }
}

bool PropertyValue::UnSerialize( GI::ByteBuffer &buf ) 
{
    bool ret = true;
    ret = buf.Pop( &m_type, sizeof( m_type ) );
    switch( m_type )
    {
      case TLONG:
          ret = buf.Pop( &m_value.lv, sizeof( m_value.lv ) );
          break;
      case TDOUBLE:
          {
              double v;
              ret = buf.Pop( &v, sizeof( v ) );
              Set( TDOUBLE, &v );
          }
          break;
      case TSTRING:
          {
              size_t len;
              buf.Pop( &len, sizeof( len ) );
              char *s = new char [len+1];
              ret = buf.Pop( s, len );
              s[len] = 0;
              std::string str( s );
              Set( TSTRING, &str );
              delete [] s;
          }
          break;
      case TGUID:
          {
              CGUID v;
              ret = buf.Pop( &v, sizeof( v ) );
              Set( TGUID, &v );
          }
          break;
      default:
          m_value.lv = 0;
    }
    return ret;
}

void PropertyValue::Free()
{
    switch( m_type )
    {
      case TDOUBLE:
          delete m_value.dv;
          break;
      case TSTRING:
          delete m_value.sv;
          break;
      case TGUID:
          delete m_value.gv;
          break;
      default:
          break;
    }
    m_type = TINVALID;
}

void PropertyValue::Set( int type, const void *val )
{
    m_type = (Type) type;
    switch( type )
    {
      case TLONG:
        m_value.lv = (long)val;
        break;
      case TDOUBLE:
        m_value.dv = new double( *(const double*)val );
        break;
      case TSTRING:
        m_value.sv = new std::string( *(const std::string*)val );
        break;
      case TGUID:
        m_value.gv = new CGUID( *(const CGUID*)val );
        break;
      default:
        m_type = TINVALID;
    }
}

PropertyValue::Stack PropertyValue::ToStackCnt( const PropertyValue &val )
{
    assert( val.m_type == TLONG );
    return val.m_value.lv;
}

// Be sure the val is valid.
PropertyValue::Identify PropertyValue::ToID( const PropertyValue &val )
{
    assert( val.m_type == TGUID );
    return *val.m_value.gv;
}

PropertyValue::Index PropertyValue::ToIndex( const PropertyValue &val )
{
    assert( val.m_type == TLONG );
    return val.m_value.lv;
}

long PropertyValue::ToLong( const PropertyValue &val )
{
    assert( val.m_type == TLONG );
    return val.m_value.lv;
}

double PropertyValue::ToDouble( const PropertyValue &val )
{
    assert( val.m_type == TDOUBLE );
    return *val.m_value.dv;
}

CGUID PropertyValue::ToGUID( const PropertyValue &val )
{
    assert( val.m_type == TGUID );
    return *val.m_value.gv;
}

std::string PropertyValue::ToString( const PropertyValue &val )
{
    assert( val.m_type == TSTRING );
    return *val.m_value.sv;
}
///////////////////////////////////////////////////////////////////////////////
const TypeSet::KeyType KeySet::IDKey( PID );
const TypeSet::KeyType KeySet::IndexKey( PINDEX );
const TypeSet::KeyType KeySet::StackCntKey( PSTACKCNT );
const TypeSet::KeyType KeySet::MaxStackCntKey( PMAX_STACKCNT );


