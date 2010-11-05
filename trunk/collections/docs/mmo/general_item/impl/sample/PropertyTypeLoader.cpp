///
/// @file PropertyTypeLoader.cpp
///
///
#include "PropertyTypeLoader.h"
#include "ValGenFuncs.h"
#include "String2Enum.h"
#include "../GIProperty.h"
#include <string>

static std::string SplitString( std::string &s, const char *split )
{
    size_t sp = s.find_first_not_of( split );
    if( sp == std::string::npos ) return "";
    size_t ep = s.find_first_of( split, sp );
    std::string rs = s.substr( sp, ep == std::string::npos ? ep : ep - sp + 1 );
    s.erase( 0, ep );
    return rs;
}

static int ReadType( const char *s )
{
    int ret = GI::PT_NULL;
    std::string str = s;
    std::string rs;
    while( ( rs = SplitString( str, " |" ) ) != "" )
    {
        ret |= String2Enum( PropertyType, rs.c_str() ); 
    }
    return ret;
}

static void ReadArg( ValGen::Param *param, const char *arg )
{
    if( param->type == ValGen::DEFAULT )
    {
        param->arg.type = String2Enum( DefaultType, arg );
    }
    else if( param->type == ValGen::RANGE )
    {
        std::string s = arg;
        std::string smin = SplitString( s, " ," );
        param->arg.range.minKey = String2Enum( EnumGoodsProperty, smin.c_str() );
        std::string smax = SplitString( s, " ," );
        param->arg.range.maxKey = String2Enum( EnumGoodsProperty, smax.c_str() );
    }
    else if( param->type == ValGen::SCRIPT )
    {
        strncpy( param->arg.file, arg, sizeof( param->arg.file ) );
    }
}

static  bool LoadProperty( TiXmlElement *elem, PropertyTypeLoader::Item *item )
{
    const char *key = elem->Attribute( "key" );
    if( !key ) return false;    
    item->key = String2Enum( EnumGoodsProperty, key );
    const char *typeAll = elem->Attribute( "type" );
    if( !typeAll ) return false;
    item->type = ReadType( typeAll );
    if( IS_DYNAMIC( item->type ) )
    {
        const char *gen = elem->Attribute( "gen" );
        if( !gen ) return false;
        item->param.type = String2Enum( ParamType, gen );
        const char *arg = elem->Attribute( "arg" );
        if( arg ) ReadArg( &item->param, arg );
    }
    return true;
}

bool PropertyTypeLoader::Load()
{
    Clear();
    CRFile *file = rfOpen( "data/goods/PropertyType.xml" );
    if( !file ) return false;
    TiXmlDocument doc;
    doc.LoadData( file->GetData(), file->GetDatalen() );
    rfClose( file );
    TiXmlElement *root = doc.RootElement();
    if( !root ) return false;
    TiXmlElement *segElem = root->FirstChildElement();
    for( ; segElem; segElem = segElem->NextSiblingElement() )
    {
        TiXmlElement *elem = segElem->FirstChildElement();
        if( elem )
        {
            Item item;
            if( !LoadProperty( elem, &item ) ) continue;
            m_items.push_back( item );
        }
    }
    return true;
}

void PropertyTypeLoader::Clear()
{
    m_items.clear();
    GI::PropertyTypeSet &pts = GI::PropertyTypeSet::getSingleton();
    pts.Clear();
}

void PropertyTypeLoader::Serialize( GI::ByteBuffer &buf ) const
{
    buf.Push( m_items.size() );
    for( ItemListT::const_iterator it = m_items.begin();
            it != m_items.end(); ++ it )
    {
        const Item &item = *it;
        buf.Push( &item, sizeof( item ) );
    }
}

bool PropertyTypeLoader::UnSerialize( GI::ByteBuffer &buf )
{
    size_t size;
    Clear();
    if( !buf.Pop( &size ) ) return false;
    m_items.reserve( size );
    for( ; size > 0; -- size )
    {
        Item item;
        if( !buf.Pop( &item, sizeof( item ) ) ) return false;
        m_items.push_back( item );
    }
    Build();
    return true;
}

void PropertyTypeLoader::Build() const
{
    GI::PropertyTypeSet &pts = GI::PropertyTypeSet::getSingleton();
    for( ItemListT::const_iterator it = m_items.begin();
            it != m_items.end(); ++ it )
    {
        const Item &item = *it;
        if( IS_DYNAMIC( item.type ) )
        {
            pts.Add( item.key, item.type, ValGen::GetFunc( item.param.type ), 
                    (void*) &item.param );
        }    
        else
        {
            pts.Add( item.key, item.type, NULL, NULL );
        }
    }
}

