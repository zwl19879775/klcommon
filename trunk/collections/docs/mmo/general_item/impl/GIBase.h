///
/// @file GIBase.h
/// @author Kevin Lynx
/// @brief GI base definitions.
///
#ifndef ___GI_BASE_H_
#define ___GI_BASE_H_

#include <stddef.h>

namespace GI
{
    class ByteBuffer
    {
    public:
        virtual ~ByteBuffer() { }
        virtual void Push( const void *d, size_t size ) = 0;
        virtual void PushWithSize( const void *d, size_t size )  { }
        virtual bool Pop( void *d, size_t size ) = 0;
        virtual bool PopFromSize( void *d ) { return false; }

        template <typename T>
        void Push( const T &d );

        template <typename T>
        bool Pop( T *d );
    };

    template <typename T>
    void ByteBuffer::Push( const T &d )
    {
        Push( &d, sizeof(T) );
    }

    template <typename T>
    bool ByteBuffer::Pop( T *d )
    {
        return Pop( d, sizeof(T) );
    }

    class SerialData
    {
    public:
        virtual void Serialize( ByteBuffer &buf ) const = 0;
        virtual bool UnSerialize( ByteBuffer &buf ) = 0;
    };
}

#endif

