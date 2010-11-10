///
/// @file ByteBufferImpl.h
///
#ifndef ___BYTE_BUFFER_IMPL_H_
#define ___BYTE_BUFFER_IMPL_H_

#include "../GIBase.h"

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

#endif

