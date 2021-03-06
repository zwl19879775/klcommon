///
/// @file ObjProtoLoader.cpp
///
///
#include "ObjProtoLoader.h"
#include "GoodsPropertyType.h"
#include <string>

#define KEYVALUE( k, v ) \
    TypeSet::KeyType( k ), TypeSet::ValueType( v ) 

static std::string ReadString( CRFile *file )
{
    long strLen;
    file->ReadData( &strLen, sizeof( strLen ) );
    char *str = new char [strLen+1];
    file->ReadData( str, strLen );
    str[strLen] = '\0';
    std::string rs = str;
    delete [] str;
    return rs;
}

template <size_t size>
void SkipByte( CRFile *file )
{
    char a[size];
    file->ReadData( a, size );
}

template <typename T>
T ReadNumber( CRFile *file )
{
    T v;
    file->ReadData( &v, sizeof( v ) );
    return v;
}

#define ReadLong(a) ReadNumber<long>(a)

static void SetIfExist( GI::ObjectProto *proto, TypeSet::KeyType key, 
        TypeSet::ValueType val )
{
    if( proto->HasProperty( key ) )
    {
        proto->SetValue( key, val );
    }
    else
    {
        proto->AddProperty( key, val );
    }
}

static bool LoadExtProperty( GI::ObjectProto *proto, CRFile *file )
{
    long cnt = ReadLong( file );
    for( long i = 0; i < cnt; ++ i )
    {
        long type = ReadNumber<short>( file );
        SkipByte<1>( file );
        SkipByte<1>( file );

        long val1 = ReadLong( file );
        long val2 = ReadLong( file );

        if( type == PMAX_STACKCNT_DEP )
        {
            SetIfExist( proto, KEYVALUE( PMAX_STACKCNT, val1 ) );
        }
        else
        {
            SetIfExist( proto, KEYVALUE( type, TypeSet::ValueType::LongPair( val1, val2 ) ) );
        }
    }
    return true;
}

static bool LoadSuitProperty( GI::ObjectProto *proto, CRFile *file )
{
    // TODO: implement this.
    long cnt = ReadLong( file );
    for( long i = 0; i < cnt; ++ i )
    {
        SkipByte<2>( file );
        SkipByte<4>( file );
        SkipByte<4>( file );
        SkipByte<2>( file );
    }
    return true;
}

static bool LoadTriggerProperty( GI::ObjectProto *proto, CRFile *file )
{
    // TODO: implement this.
    long cnt = ReadLong( file );
    for( long i = 0; i < cnt; ++ i )
    {
        SkipByte<2>( file );
        SkipByte<4>( file );
        SkipByte<4>( file );
        SkipByte<2>( file );
    }
    return true;
}

static void AddAdapterProperty( GI::ObjectProto *proto )
{
    proto->AddProperty( KEYVALUE( PMAX_STACKCNT, (long) DEFAULT_MAX_STACKCNT ) );
    proto->AddProperty( KEYVALUE( PSTACKCNT, 0L ) );
    proto->AddProperty( KEYVALUE( PID, NULL_GUID ) );
    proto->AddProperty( KEYVALUE( PMAKER_NAME, "" ) );
}

static bool LoadProto( GI::ObjectProto *proto, CRFile *file )
{
    long index = ReadLong( file );
    proto->AddProperty( KEYVALUE( PINDEX, index ) );

    std::string origName = ReadString( file );
    proto->AddProperty( KEYVALUE( PORIGNAME, origName ) );

    std::string name = ReadString( file );
    proto->AddProperty( KEYVALUE( PNAME, name ) );

    SkipByte<1>( file );

    long goldPrice = ReadLong( file );
    proto->AddProperty( KEYVALUE( PGOLD_PRICE, goldPrice ) );

    long sliverPrice = ReadLong( file );
    proto->AddProperty( KEYVALUE( PSILVER_PRICE, sliverPrice ) );

    long type = ReadLong( file );
    proto->AddProperty( KEYVALUE( PTYPE, type ) );

    long pic = ReadLong( file );
    proto->AddProperty( KEYVALUE( PUI_PICID, pic ) );

    pic = ReadLong( file );
    proto->AddProperty( KEYVALUE( PRGN_PICID, pic ) );

    pic = ReadLong( file );
    proto->AddProperty( KEYVALUE( PEQUIP_PICID, pic ) );

    SkipByte<4*3>( file );
    SkipByte<1>( file );
    SkipByte<4>( file );

    std::string desc = ReadString( file );
    proto->AddProperty( KEYVALUE( PDESC, desc ) );

    // NOTE: 
    AddAdapterProperty( proto );

    LoadExtProperty( proto, file );
    LoadSuitProperty( proto, file );
    LoadTriggerProperty( proto, file );
    
    return true;
}

bool ObjProtoLoader::Load( GI::ObjProtoFactory *fac, void *u )
{
    CRFile *file = rfOpen( "data/goods/goodslist.dat" );
    if( !file ) return false;
    char mark[6] = { 0 };
    file->ReadData( mark, 5 );
    if( strcmp( mark, "GOODS" ) ) return false;
    long version;
    file->ReadData( &version, sizeof( version ) );
    long cnt;
    file->ReadData( &cnt, sizeof( cnt ) );
    for( long i = 0; i < cnt; ++ i )
    {
        GI::ObjectProto *proto = new GI::ObjectProto();
        if( !LoadProto( proto, file ) ) delete proto; 
        else
        {
            long index = TypeSet::ValueType::ToLong( proto->GetValue( 
                        TypeSet::KeyType( PINDEX ) ) );
            fac->AddProperty( index, proto );
        }
    }
    rfClose( file );
    return true;
}

