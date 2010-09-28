///
/// @file GIBase.h
/// @author Kevin Lynx
/// @brief GI base definitions.
///
#ifndef ___GI_BASE_H_
#define ___GI_BASE_H_

namespace GI
{
    class ByteBuffer
    {
    public:
        virtual void Push( const void *d, size_t size ) = 0;
        virtual bool Pop( void *d, size_t size ) = 0;
    };

    class SerialData
    {
    public:
        virtual void Serialize( ByteBuffer &buf ) = 0;
        virtual bool UnSerialize( ByteBuffer &buf ) = 0;
    };
}

#endif

