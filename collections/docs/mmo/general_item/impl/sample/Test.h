///
/// @file Test.h
///
///
#ifndef ___TEST_H_
#define ___TEST_H_

#include <stdlib.h>

#define CGUID long long  
#define NULL_GUID 0L

enum
{
    TYPE_PLAYER, TYPE_REGION, TYPE_SESSION, TYPE_GOODS
};

enum
{
    PACKET,
};

enum
{
    PEI_PACKET, PEI_PACK, PEI_PACK1, PEI_PACK2, PEI_PACK3, PEI_PACK4,
    PEI_DEPOT, PEI_DEPOTPACK, PEI_DEPOT1, PEI_DEPOT2, PEI_DEPOT3, PEI_DEPOT4,
};

#define random( min, max ) ( rand()%(max-min) + min )

class CRFile
{
public:
    char *GetData();
    long GetDatalen();
    void ReadData( void *, long size );
};

CRFile *rfOpen( const char *file );
void rfClose( CRFile *file );

class TiXmlElement;
class TiXmlDocument
{
public:
    bool LoadData( char *buf, long size );
    TiXmlElement *RootElement();
};

class TiXmlElement
{
public:
    TiXmlElement *FirstChildElement();
    TiXmlElement *NextSiblingElement();
    const char *Attribute( const char *s );
};

class DBReadSet
{
public:
    struct Temp
    {
        void *pDBPtr;
    } *pDBReadParam;

    void GetBufferFromByteArray( void *d, size_t size );
};

class DBWriteSet
{
public:
    void AddToByteArray( const void *u, size_t size );
};

class CMessage
{
public:
    CMessage( long t );

    template <typename T>
    void Add( T t );

    long GetLong();

    void GetGUID( CGUID &id );

    bool GetDBReadSet( DBReadSet &db );

    bool GetDBWriteSet( DBWriteSet &db );

    void SendToPlayer( const CGUID &id );
};

enum
{
    MSG_S2C_CONTAINER_OBJECT_MOVE,
};

class PlayerContainer;
class CPlayer
{
public:
    PlayerContainer &GetContainer();
};

class Game
{
public:
    CPlayer *FindPlayer( const CGUID &id );
};

Game *GetGame();

#endif

