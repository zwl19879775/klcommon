///
/// @file ByteBufferImpl.h
///
#ifndef ___BYTE_BUFFER_IMPL_H_
#define ___BYTE_BUFFER_IMPL_H_

#include "../GIBase.h"
#include <vector>

class ReadBuffer : public GI::ByteBuffer
{
public:
    ReadBuffer( DBReadSet &db ) : m_db( db ) { }

    virtual ~ReadBuffer() { }

    virtual bool Pop( void *d, size_t size ) 
    {
        if( m_db.pDBReadParam->pDBPtr == NULL ) return false;
        memset( d, 0, size );
        m_db.GetBufferFromByteArray( d, size );
        return true;
    }

    virtual bool PopFromSize( void *d )
    {
        size_t size;
        if( !Pop( &size, sizeof( size ) ) ) return false;
        return Pop( d, size );
    }

    virtual void Push( const void *u, size_t size ) { }
private:
    DBReadSet &m_db;
};

class WriteBuffer : public GI::ByteBuffer
{
public:
    WriteBuffer( DBWriteSet &db ) : m_db( db ) { }

    virtual ~WriteBuffer() { }

    virtual void Push( const void *u, size_t size ) 
    {
        m_db.AddToByteArray( u, size );
    }

    virtual void PushWithSize( const void *u, size_t size )
    {
        Push( &size, sizeof( size ) );
        Push( u, size );
    }

    virtual bool Pop( void *d, size_t size ) { return false; }
private:
    DBWriteSet &m_db;
};

/// Store data in this first.
class StoreWriteBuffer : public GI::ByteBuffer
{
public:
    typedef std::vector<unsigned char> DataBlockT;
public:
    StoreWriteBuffer( size_t reserved = 512 )
    {
        m_d.reserve( reserved );
    }

    virtual ~StoreWriteBuffer() { }

    virtual void Push( const void *u, size_t size )
    {
        const unsigned char *d = (const unsigned char*) u; 
        for( ; size > 0; -- size, ++ d )
        {
            m_d.push_back( *d );
        }
    }

    virtual void PushWithSize( const void *u, size_t size )
    {
        Push( &size, sizeof( size ) );
        Push( u, size );
    }

    virtual bool Pop( void *d, size_t size ) { return false; }

    size_t Size() const { return m_d.size(); }

    const void *Data() const { return &m_d[0]; }
private:
    DataBlockT m_d;
};

#endif

